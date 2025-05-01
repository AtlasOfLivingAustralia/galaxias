#' Check an archive against Darwin Core standards
#' 
#' @description
#' Check whether schema and EML files (i.e. `meta.xml` and `eml.xml`) are
#' valid with the `delma` package; csv files are checked with the `corella`
#' package. This is a wrapper to two other packages.
#' @param source (string) A directory containing the files to be published, or 
#' optionally a `.zip` file built from the same (i.e. with `build_archive()`). 
#' Defaults to the `data-publish` folder within the current working directory.
#' @seealso `validate_archive()` which runs checks via API, rather than locally.
#' @returns Invisibly returns a tibble to the workspace containing check 
#' results; but primarily called for the side-effect of generating a report in 
#' the console.
#' @export
check_archive <- function(source = "data-publish"){ # add `file` arg for consistency with `check_eml()`
  # browser()
  if(!file.exists(source)){
    glue::glue("File or directory '{.file {source}}' not found.") |>
      cli::cli_abort()
  }else{
    if(grepl(".zip$", source)){
      file_list <- utils::unzip(source, list = TRUE)
    }else{
      file_list <- list.files(source)
    }
  }
  
  check_files(glue::glue("{source}/{file_list}"))
}

#' Internal function to check all files
#' @noRd
#' @keywords Internal
check_files <- function(filenames){
  purrr::map(filenames, 
      \(a){
        switch(a, 
               "occurrences.csv" = {
                 readr::read_csv(a, show_col_types = FALSE) |>
                                    corella::check_dataset()
                 },
               "meta.xml" = {delma::check_metadata(a)},
               "eml.xml" = {delma::check_metadata(a)}
       )
  }) |>
    invisible()
}

corella::check_dataset(readr::read_csv("data-publish/events.csv"))
suggest_workflow()
