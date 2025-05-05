#' Check whether contents of folder directory meet Darwin Core Standard
#' 
#' @description
#' Supply a folder containing files for a Darwin Core Archive to check whether 
#' files meet Darwin Core Standard. `check_archive()` runs 
#' [corella::check_dataset()] on `occurrences.csv` and `events.csv` files, and 
#' [delma::check_metadata()] on `eml.xml` and `meta.xml` files. These 
#' functions run tests to determine whether data and metadata pass 
#' Darwin Core Standard criteria. 
#' @param source (string) A directory containing the files to be published, or 
#' optionally a `.zip` file built from the same (i.e. with `build_archive()`). 
#' Defaults to the `data-publish` folder within the current working directory.
#' @seealso `validate_archive()` checks a Darwin Core Archive via a GBIF API, 
#' rather than locally.
#' @returns Invisibly returns a tibble to the workspace containing check 
#' results; but primarily called for the side-effect of generating a report in 
#' the console.
#' @export
check_archive <- function(source = "data-publish"){ 

  if(!file.exists(source)){
    c("File or directory {.file {source}} not found.") |>
      cli::cli_abort()
  }else{
    if(grepl(".zip$", source)){
      bullets <- c(
        "Must supply a source folder name.",
        i = "`check_archive()` only accepts a folder name, not a zip file."
        ) |>
        cli::cli_bullets() |>
        cli::cli_fmt()
      cli::cli_abort(bullets)
      # file_list <- utils::unzip(source, list = TRUE) |>
      #   dplyr::pull(Name)
    }else{
      file_list <- list.files(source)
    }
  }
  
  check_files(source, file_list)
}

#' Internal function to check all files
#' @noRd
#' @keywords Internal
check_files <- function(source, filenames){
  
  purrr::map(filenames, 
      \(a){
        switch(a, 
               "occurrences.csv" = {
                 cli::cli_h1("Test {.file occurrences.csv}")
                 wait(1.3)
                 readr::read_csv(glue::glue("{source}/{a}"), show_col_types = FALSE) |>
                   corella::check_dataset()
                 },
               "events.csv" = {
                 cli::cli_h1("Test {.file events.csv}")
                 wait(1.3)
                 readr::read_csv(glue::glue("{source}/{a}"), show_col_types = FALSE) |>
                   corella::check_dataset()
                 },
               "meta.xml" = {
                 cli::cli_h1("Test {.file meta.xml}")
                 wait(1.3)
                 delma::check_metadata(glue::glue("{source}/{a}"))},
               "eml.xml" = {
                 cli::cli_h1("Test {.file eml.xml}")
                 wait(1.3)
                 delma::check_metadata(glue::glue("{source}/{a}"))
                 }
       )
  }) |>
    invisible()
}

