#' Check an archive against Darwin Core standards
#' 
#' This is a wrapper to two other packages; schema and EML files (i.e. xml) are
#' checked with the `delma` package; csv files are checked with the `corella`
#' package.
#' @param x (string) A directory containing the files to be published, or 
#' optionally a `.zip` file built from the same (i.e. with `build_archive()`). 
#' Defaults to the `data-publish` folder within the current working directory.
#' @seealso `validate_archive()` which runs checks via API, rather than locally.
#' @returns Invisibly returns a tibble to the workspace containing check 
#' results; but primarily called for the side-effect of generating a report in 
#' the console.
#' @export
check_archive <- function(x = "data-publish"){ # add `file` arg for consistency with `check_eml()`
  if(!file.exists(x)){
    glue::glue("file or directory '{.file x}' not found") |>
      cli::cli_abort()
  }else{
    if(grepl(".zip$")){
      file_list <- utils::unzip(x, list = TRUE)
    }else{
      file_list <- list.files(x)
    }
  }
  check_files(file_list)
}

#' Internal function to check all files
#' @noRd
#' @keywords Internal
check_files <- function(filenames){
  purrr::map(filenames, 
      \(a){
        switch(a, 
               "occurrences.csv" = {readr::read_csv(a) |>
                                    corella::check_dataset()},
               "meta.xml" = {delma::check_eml(a)},
               "eml.xml" = {delma::check_eml(a)}
       )
  }) |>
    invisible()
}

