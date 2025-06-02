#' Check whether contents of directory comply with the Darwin Core Standard
#' 
#' @description
#' Checks that files in a directory meet Darwin Core Standard. The path of the
#' directory is set to the `directory` specified in `[galaxias_config()]`. 
#' Users can run `galaxias_config()` to print the current specified `directory`.
#' 
#' `check_archive()` runs 
#' [corella::check_dataset()] on `occurrences.csv` and `events.csv` files, and 
#' [delma::check_metadata()] on `eml.xml` and `meta.xml` files, if they are 
#' present. These `check_` functions run tests to determine whether data and 
#' metadata pass Darwin Core Standard criteria. 
#' @returns Invisibly returns a tibble to the workspace containing check 
#' results; but primarily called for the side-effect of generating a report in 
#' the console.
#' @examples \donttest{
#' # Run checks csv or xml files in specified directory
#' # Defaults to folder data-publish/ 
#' check_directory()
#' 
#' }
#' @seealso `check_archive()` checks a Darwin Core Archive via a GBIF API, 
#' rather than locally.
#' @export
check_directory <- function(){ 
  
  # run checks on `directory`
  directory <- potions::pour("directory",
                          .pkg = "galaxias")
  if(!fs::dir_exists(directory)){
    cli::cli_abort(c("Directory {.file {directory}} does not exist."))
  }
  
  file_list <- list.files(directory)
  
  cli::cli_alert_info("Testing files in {.file {directory}}")

  check_files(directory, file_list)
}

#' Internal function to check all files in `directory`
#' @noRd
#' @keywords Internal
check_files <- function(directory, filenames){
  
  purrr::map(filenames, 
      \(a){
        switch(a, 
               "occurrences.csv" = {
                 cli::cli_h1("Test {.file occurrences.csv}")
                 wait(1.3)
                 readr::read_csv(glue::glue("{directory}/{a}"), show_col_types = FALSE) |>
                   corella::check_dataset()
                 },
               "events.csv" = {
                 cli::cli_h1("Test {.file events.csv}")
                 wait(1.3)
                 readr::read_csv(glue::glue("{directory}/{a}"), show_col_types = FALSE) |>
                   corella::check_dataset()
                 },
               "meta.xml" = {
                 cli::cli_h1("Test {.file meta.xml}")
                 wait(1.3)
                 delma::check_metadata(glue::glue("{directory}/{a}"))},
               "eml.xml" = {
                 cli::cli_h1("Test {.file eml.xml}")
                 wait(1.3)
                 delma::check_metadata(glue::glue("{directory}/{a}"))
                 }
       )
  }) |>
    invisible()
}

