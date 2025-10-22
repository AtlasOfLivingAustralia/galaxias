#' Use standardised data in a Darwin Core Archive
#' 
#' @description
#' Once data conform to Darwin Core Standard, `use_data()` makes it 
#' easy to save data in the correct place for building a Darwin Core Archive 
#' with [build_archive()].
#' 
#' `use_data()` is an all-in-one function for accepted data types "occurrence", 
#' "event" and "multimedia". `use_data()` attempts to detect and save the 
#' correct data type based on the provided `tibble`/`data.frame`.
#' Alternatively, users can call the underlying functions 
#' [use_data_occurrences()] or [use_data_events()] to 
#' specify data type manually.
#' @details
#' This function saves data in the `data-publish` folder. It will create that
#' folder if it is not already present.
#' 
#' Data type is determined by detecting type-specific column names in 
#' supplied data. 
#' * Event: (`eventID`, `parentEventID`, `eventType`)
#' * Multimedia: not yet supported
#' @param ... Unquoted name of `tibble`/`data.frame` to save.
#' @param overwrite By default, `use_data()` will not 
#'   overwrite existing files. If you really want to do so, set this to `TRUE`. 
#' @param quiet Whether to message about what is happening. Default is set to 
#' @returns Does not return anything to the workspace; called for the side-effect
#' of saving a `.csv` file to `/data-publish`.
#' @seealso [use_metadata()] to save metadata to `/data-publish`.
#' @examples
#' \dontshow{
#' .old_wd <- getwd()
#' temp_dir <- tempdir()
#' usethis::proj_set(path = temp_dir, force = TRUE) # without this use_data_occurrences() will fail
#' setwd(temp_dir)
#' }
#' 
#' # Build an example dataset
#' df <- tibble::tibble(
#'   occurrenceID = c("a1", "a2"),
#'   species = c("Eolophus roseicapilla", "Galaxias truttaceus"))
#'
#' # The default function *always* asks about data type
#' if(interactive()){
#'   use_data(df)
#' }
#' 
#' # To manually specify the type of data - and avoid questions in your 
#' # console - use the underlying functions instead
#' use_data_occurrences(df, quiet = TRUE)
#' 
#' # Check that file has been created
#' list.files("data-publish")
#' 
#' # returns "occurrences.csv" as expected
#' 
#' \dontshow{
#' unlink("data-publish", recursive = TRUE)
#' usethis::proj_set(path = .old_wd, force = TRUE)
#' setwd(.old_wd)
#' }
#' @export
use_data <- function(...,
                     overwrite = FALSE,
                     quiet = FALSE) {
  
  user_data <- list(...)
  
  if(length(user_data) < 1L){
    cli::cli_abort("Nothing to save.")
  }
  
  # apply rules to number & classes of objects
  user_data <- check_is_data(user_data)
  
  # Check if data is dwc compliant here?
  
  # check whether user data.frame contains occurrences, events or multimedia
  type <- check_data_type(user_data)
  
  switch_data_type(user_data,
                   type,
                   overwrite,
                   quiet)
}

#' Internal function to check for tibbles
#' @noRd
#' @keywords Internal
check_is_data <- function(obj, error_call = rlang::caller_env()) {
  # browser()
  # check whether more than one data.frame/tibble is supplied
  tibble_check <- obj |> 
    purrr::map(
      \(elements)
      inherits(elements, "data.frame")
      ) |>
    unlist()
  n_tibbles <- sum(tibble_check)
  
  # apply rules
  if(n_tibbles > 1) {
    cli::cli_abort("Can only supply one `tibble`/`data.frame` to save.",
                   call = error_call)
  }else if(n_tibbles < 1){
    cli::cli_abort("Must supply a `tibble`/`data.frame` to save.",
                   call = error_call)    
  }else{
    obj[[which(tibble_check)]]
  }
}

#' Identifies data type based on column names, then checks with user
#' @noRd
#' @keywords Internal
check_data_type <- function(data, error_call = rlang::caller_env()) {
  
  if (any(colnames(data) %in% c("eventID", "parentEventID"))) {
    # Event
    type <- "event"
    file_name <- "events.csv"
    not <- "occurrence"
  } else {
    # Occurrence
    type <- "occurrence"
    file_name <- "occurrences.csv"
    not <- "event"
  }
  
  if (rlang::is_interactive()) {
    choice <- cli_menu(
      "Data identified as type {.field {type}} and will be saved as {.file {file_name}}",
      "Is this correct? (or 0 to exit)",
      choices = c("Yes", "No")
    )

    if (choice == 1) {
      return(type)
    } else {
      cli::cli_inform(c(
        i = "To specify data type, use bespoke data functions instead e.g. ",
        " " = "{.fn use_data_occurrences}, {.fn use_data_events}."
      ))
      # exits process quietly
      invokeRestart("abort")
    }
    invisible()
  } else {
    return(type)
  }
}

#' Switch to occurrence, event or multimedia type `use_data()` functions
#' @noRd
#' @keywords Internal
switch_data_type <- function(user_data, type, overwrite, quiet) {

  switch(type,
         "occurrence" = use_data_occurrences(user_data, 
                                             overwrite = overwrite, 
                                             quiet = quiet),
         "event" = use_data_events(user_data, 
                                   overwrite = overwrite, 
                                   quiet = quiet)
         # "multimedia" = {use_data_multimedia(data)} # multimedia not yet supported
         )
}

#' Use occurrence-type data in a Darwin Core Archive
#' @param df A `tibble`/`data.frame` to save.
#' @param overwrite By default, `use_data_occurrences()` will not 
#'   overwrite existing files. If you really want to do so, set this to `TRUE`. 
#' @param quiet Whether to message about what is happening. Default is set to 
#'  `FALSE`. 
#' @rdname use_data
#' @export
use_data_occurrences <- function(df, 
                                 overwrite = FALSE,
                                 quiet = FALSE) {
  
  # check if it's a dataframe
  if (!tibble::is_tibble(df) & !is.data.frame(df)) {
    cli::cli_abort("Must provide a `tibble`/`data.frame`.")
  }
  
  directory <- check_publish_directory(quiet = quiet)
  file_path <- fs::path(directory, "occurrences.csv")
  write_data_file(file_path,
                  df,
                  overwrite,
                  quiet)
}

#' Use event-type data in a Darwin Core Archive
#' @param df A `tibble`/`data.frame` to save.
#' @param overwrite By default, `use_data_events()` will not 
#'   overwrite existing files. If you really want to do so, set this to `TRUE`. 
#' @param quiet Whether to message about what is happening. Default is set to 
#'  `FALSE`. 
#' @rdname use_data
#' @export
use_data_events <- function(df,
                            overwrite = FALSE,
                            quiet = FALSE) {
  
  # check if it's a dataframe
  if (!tibble::is_tibble(df) & !is.data.frame(df)) {
    cli::cli_abort("Must provide a `tibble`/`data.frame`.")
  }
  
  directory <- check_publish_directory(quiet = quiet)
  file_path <- fs::path(directory, "events.csv")
  write_data_file(file_path,
                  df, 
                  overwrite,
                  quiet)
  
}

#' Save data to file path
#' @noRd
#' @keywords Internal
write_data_file <- function(file_path,
                            data, 
                            overwrite = FALSE,
                            quiet = FALSE,
                            error_call = rlang::caller_env()) {
  
  if(fs::file_exists(file_path)){
    if(overwrite){
      if(!quiet){
        cli::cli_progress_step("Overwriting {.file {file_path}}.")
      }
      
      readr::write_csv(data, file = file_path)
      
      if(!quiet){
        cli::cli_progress_done()
      }
    }else{
      c("File {.file {file_path}} already exists.",
        i = "Use `overwrite = TRUE` to overwrite.") |>
        cli::cli_inform()     
    }
  }else{
    if(!quiet){
      cli::cli_progress_step("Writing {.file {file_path}}.")
    }
    
    readr::write_csv(data, file = file_path)
    
    if(!quiet){
      cli::cli_progress_done()
    }
  }
  
}
