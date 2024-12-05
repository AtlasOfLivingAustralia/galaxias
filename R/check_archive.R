#' Check an archive against Darwin Core standards
#' 
#' This is a wrapper to two other packages; schema and EML files (i.e. xml) are
#' checked with the `paperbark` package; csv files are checked with the `corella`
#' package.
#' @param x (string) A directory containing the files to be published, or 
#' optionally a `.zip` file built from the same (i.e. with `build_archive()`). 
#' Defaults to the `data` folder within the current working directory.
#' @seealso `validate_archive()` which runs checks via API, rather than locally.
#' @returns Invisibly returns a tibble to the workspace containing check 
#' results; but primarily called for the side-effect of generating a report in 
#' the console.
#' @importFrom utils unzip
#' @export
check_archive <- function(x = "data"){
  if(!file.exists(x)){
    abort(glue("file or directory '{x}' not found"))
  }else{
    if(grepl(".zip$")){
      file_list <- unzip(x, list = TRUE)
    }else{
      file_list <- list.files(x)
    }
  }
  check_files(file_list)
}

#' Internal function to check all files
#' @importFrom corella check_occurrences
#' @importFrom paperbark check_eml
#' @importFrom purrr map
#' @importFrom readr read_csv
#' @noRd
#' @keywords Internal
check_files <- function(filenames){
  map(filenames, 
      \(a){
        switch(a, 
               "occurrences.csv" = {read_csv(a) |>
                                    check_occurrences()},
               "meta.xml" = {check_eml(a)},
               "eml.xml" = {check_eml(a)}
       )
  }) |>
    invisible()
}

