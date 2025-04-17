#' Save standardised occurrence-type data
#' 
#' @description
#' Once data conform to Darwin Core Standard, `use_data_occurrences()` makes it 
#' easy to save data in the correct place for building a Darwin Core Archive 
#' with [build_archive()].
#' @details
#' This function saves data in the `data-publish` folder, which is the default 
#' folder that `galaxias` uses to build a Darwin Core Archive using 
#' [build_archive()].
#' @param ... (string) Unquoted name of `tibble`/`data.frame` to save.
#' @param overwrite (logical) By default, `use_data_occurrences()` will not 
#'   overwrite existing files. If you really want to do so, set this to `TRUE`. 
#' @return Invisibly returns the location of the saved csv file.
#' @seealso [use_data_events()]
#' @export
use_data_occurrences <- function(...,
                                 overwrite = FALSE) {
  
  obj <- get_objs_from_dots(dots(...))
  user_data <- check_is_data(obj)
  
  # Check if data is dwc compliant here?
  
  # There should be some kind of `check_is_occ_based()` function to make sure 
  #   users are supplying the correct data for this operation.
  
  usethis::use_directory("data-publish")
  file_path <- fs::path("data-publish", "occurrences.csv")
  
  if(file.exists(file_path)){
    if(overwrite){
      cli::cli_progress_step("Overwriting existing file `{.file {file_path}}`.")        
      readr::write_csv(user_data, file = file_path)
      cli::cli_progress_done()
    }else{
      c("{.file {file_path}} already exists.",
        i = "Set `overwrite = TRUE` to overwrite existing file.") |>
        cli::cli_inform()     
    }
  }else{
    cli::cli_progress_step("Writing {.file {file_path}}.")
    readr::write_csv(user_data, file = file_path)
    cli::cli_progress_done()
  }
  
}

#' Save standardised event-type data
#' 
#' @description
#' Once data conform to Darwin Core Standard, `use_data_events()` makes it 
#' easy to save data in the correct place for building a Darwin Core Archive 
#' with [build_archive()].
#' @details
#' This function saves data in the `data-publish` folder, which is the default 
#' folder that `galaxias` uses to build a Darwin Core Archive using 
#' [build_archive()].
#' @param ... (string) Unquoted name of `tibble`/`data.frame` to save.
#' @param overwrite (logical) By default, `use_data_events()` will not 
#'   overwrite existing files. If you really want to do so, set this to `TRUE`. 
#' @return Invisibly returns the location of the saved csv file.
#' @seealso [use_data_occurrences()]
#' @export
use_data_events <- function(...,
                            overwrite = FALSE) {
  
  obj <- get_objs_from_dots(dots(...))
  user_data <- check_is_data(obj)
  
  # Check if data is dwc compliant here?
  
  # `check_is_events_based()`?
  
  usethis::use_directory("data-publish")
  file_path <- fs::path("data-publish", "events.csv")
  
  if(file.exists(file_path)){
    if(overwrite){
      cli::cli_progress_step("Overwriting existing file `{.file {file_path}}`.")        
      readr::write_csv(user_data, file = file_path)
      cli::cli_progress_done()
    }else{
      c("{.file {file_path}} already exists.",
        i = "Set `overwrite = TRUE` to overwrite existing file.") |>
        cli::cli_inform()     
    }
  }else{
    cli::cli_progress_step("Writing {.file {file_path}}.")
    readr::write_csv(user_data, file = file_path)
    cli::cli_progress_done()
  }
  
}


get_objs_from_dots <- function(.dots, error_call = rlang::caller_env()) {
  if (length(.dots) == 0L) {
    cli::cli_abort("Nothing to save.",
                   call = error_call)
  }

  is_name <- vapply(.dots, is.symbol, logical(1))
  if (!all(is_name)) {
    cli::cli_abort("Can only save existing named objects.",
                   call = error_call)
  }

  return(.dots)
}

check_is_data <- function(obj, error_call = rlang::caller_env()) {
  
  if (length(obj) > 1 ) {
    cli::cli_abort("Can only supply one object to save.",
                   call = error_call)
  }
  
  obj <- obj[[1]] |> eval()
  
  if(!any(inherits(obj, c("tbl_df", "tbl", "data.frame")))) {
    cli::cli_abort("Must supply a `tibble`/`data.frame` to save.",
                   call = error_call)
  }
  
  obj
  
}
