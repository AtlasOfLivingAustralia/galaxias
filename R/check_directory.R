#' Check whether contents of directory comply with the Darwin Core Standard
#' 
#' @description
#' Checks that files in the `data-publish` directory meet Darwin Core Standard. 
#' `check_directory()` runs [corella::check_dataset()] on `occurrences.csv` and 
#' `events.csv` files, and [delma::check_metadata()] on the `eml.xml` 
#' file, if they are present. These `check_` functions run tests to determine 
#' whether data and metadata pass Darwin Core Standard criteria. 
#' @returns Doesn't return anything; called for the side-effect of generating a 
#' report in the console.
#' @seealso [check_archive()] checks a Darwin Core Archive via a GBIF API, 
#' rather than locally.
#' @export
check_directory <- function(){ 
  
  # run checks on `directory`
  directory <- "data-publish"
  if(!fs::dir_exists(directory)){
    cli::cli_abort(c("Directory {.file {directory}} does not exist."))
  }
  
  file_list <- list.files(directory)
  
  cli::cli_alert_info("Testing files in {.file {directory}}")

  check_files(directory, file_list)
  return(invisible())
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
               ## `delma::check_metadata()` only checks for valid EML at present, not schema docs
               ## Uncomment this once we have a sensible `check_` function for this data type
               # "meta.xml" = {
               #   cli::cli_h1("Test {.file meta.xml}")
               #   wait(1.3)
               #   delma::check_metadata(glue::glue("{directory}/{a}"))},
               "eml.xml" = {
                 cli::cli_h1("Test {.file eml.xml}")
                 wait(1.3)
                 delma::check_metadata(glue::glue("{directory}/{a}"))
                 }
       )
  }) |>
    invisible()
}

