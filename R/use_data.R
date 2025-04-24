#' Use standardised data in a Darwin Core Archive
#' 
#' @description
#' Once data conform to Darwin Core Standard, `use_data()` makes it 
#' easy to save data in the correct place for building a Darwin Core Archive 
#' with [build_archive()].
#' @details
#' This function saves data in the `data-publish` folder, which is the default 
#' folder that `galaxias` uses to build a Darwin Core Archive using 
#' [build_archive()].
#' @param ... (string) Unquoted name of `tibble`/`data.frame` to save.
#' @param type (string) Data type. Designates whether data are saved as 
#'   `occurrences.csv` or `events.csv`. `type` is determined by detecting 
#'   Event-type column names in data (only valid in an Event-type dataset).
#' @param overwrite (logical) By default, `use_data()` will not 
#'   overwrite existing files. If you really want to do so, set this to `TRUE`. 
#' @return Invisibly returns the location of the saved csv file.
#' @seealso [use_metadata()]
#' @export
use_data <- function(...,
                     type = c("occurrence", "event"),
                     overwrite = FALSE) {
  
  obj <- get_objs_from_dots(dots(...))
  user_data <- check_is_data(obj)
  
  # Check if data is dwc compliant here?
  
  type <- check_data_type(user_data)
  
  usethis::use_directory("data-publish") # add folder if it's not already there
  
  if(type == "occurrences") {
    file_path <- fs::path("data-publish", "occurrences.csv")
  } else {
    file_path <- fs::path("data-publish", "events.csv")
  }
  
  
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


check_data_type <- function(data, error_call = rlang::caller_env()) {
  
  if (any(colnames(data) %in% c("eventID", "parentEventID"))) {
    type <- "event"
    not <- "occurrence"
  } else {
    type <- "occurrence"
    not <- "event"
  }
  bullets <- c("Data identified as {type}-type.",
               i = cli::col_grey("If this is incorrect, use {.code use_date(type = \"{not}\")} instead."))
  cli::cli_inform(bullets)
  
  return(type)
  
}