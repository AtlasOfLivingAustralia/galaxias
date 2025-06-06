#' Build a Darwin Core Archive from a folder
#' 
#' @description
#' A Darwin Core archive is a zip file containing a combination of 
#' data and metadata. `build_archive()` constructs this zip file in the parent
#' directory. The function assumes that all necessary files have been 
#' pre-constructed, and can be found inside the `"data-publish"` directory
#' with no additional or redundant information. Structurally, `build_archive()` 
#' is similar to `devtools::build()`, in the sense that it takes a repository 
#' and wraps it for publication.
#' @details
#' This function looks for three types of objects in the `data-publish` 
#' directory:
#' 
#'  * Data 
#'    
#'    One or more csv files named `occurrences.csv`, `events.csv` and/or 
#'    `multimedia.csv`.
#'    These csv files contain data standardised using Darwin Core Standard 
#'    (see [corella::corella-package()] for details). A `data.frame`/`tibble` 
#'    can be added to the correct folder using [use_data()].
#'  
#'  * Metadata
#'  
#'    A metadata statement in `EML` format with the file name `eml.xml`. 
#'    Completed metadata statements written markdown as `.Rmd` or `qmd` files 
#'    can be converted and saved to the correct folder using [use_metadata()]. 
#'    Create a new template with [use_metadata_template()].
#'  
#'  * Schema 
#'  
#'    A 'schema' document in xml format with the file name `meta.xml`. 
#'    `build_archive()` will detect whether this file is present and build a 
#'    schema file if missing. This file can also be constructed 
#'    separately using [use_schema()].
#' 
#' @param file The name of the file to be built in the parent directory.
#' Should end in `.zip`.
#' @param overwrite (logical) Should existing files be overwritten? Defaults to 
#' `FALSE`.
#' @param quiet (logical) Whether to suppress messages about what is happening. 
#' Default is set to `FALSE`; i.e. messages are shown.
#' @return Doesn't return anything; called for the side-effect of building a 
#' 'Darwin Core Archive' (i.e. a zip file).
#' @seealso [use_data()], [use_metadata()], [use_schema()]
#' @export
build_archive <- function(file = "dwc-archive.zip",
                          overwrite = FALSE,
                          quiet = FALSE) {
  
  check_filename(file)
  
  if(!quiet){
    cli::cli_alert_info("Building Darwin Core Archive")
  }
  
  if(!quiet){
    progress_update("Detecting files..."); wait(.1)
  }
  
  # Check for and display which files are in source folder
  # Users will hit an error if they are 
  #  - missing all data files, or
  #  - missing an EML metadata statement
  source <- "data-publish"
  if(!fs::dir_exists(source)){
    cli::cli_abort("Directory {.file {source}} does not exist.")
  }
  
  files_in <- find_data(source, quiet = quiet)
  
  # If schema file is missing, offer to build it
  if(!any(files_in %in% glue::glue("{source}/meta.xml"))){
    build_schema_internal(source = source, 
                          quiet = quiet)
  }
  
  if(!quiet) {
    progress_update("Creating zip file...")
  }
  
  # run checks on `archive`
  archive <- fs::path_abs(glue::glue("../{file}"))
  
  if(fs::file_exists(archive)){
    if(overwrite){
      if(!quiet){
        cli::cli_progress_step(c("Overwriting {.file {archive}}."))
      }
      zip::zip(zipfile = archive, 
               files = files_in,
               mode = "cherry-pick")
    }else{
      cli::cli_abort(c("Darwin Core Archive already exists and has not been overwritten.",
                       i = "Found existing archive {.path {archive}}",
                       i = "Use `overwrite = TRUE` to overwrite."))
    }
  }else{
    if(!quiet){
      cli::cli_progress_step(c("Writing {.file {file}}"))
    }
    zip::zip(zipfile = archive, 
             files = files_in,
             mode = "cherry-pick")
  }

  if(!quiet){cli::cli_progress_done()}

  cli::cli_inform(c("Saved {.file {file}} to the parent directory of the working directory.",
                    "*" = cli::col_grey("Path: {.file {fs::path(archive)}}")))
  
  return(invisible())
}

#' Internal function to automatically build_schema() inside build_archive()
#' @noRd
#' @keywords Internal
build_schema_internal <- function(source, quiet){
  # offer user menu to confirm if not in batch run (testthat or knitr)
  if(rlang::is_interactive() & !quiet){ 
    
    choice <- cli_menu(
      c(" ",
        "No schema ({.file meta.xml}) file detected.", 
        "This is a required file in a Darwin Core Archive.", 
        " "),
      "Do you want to build a schema file now? (0 to exit)",
      choices = c("Yes", "No")
    )
    
    if (choice == 1) {
      use_schema(quiet = quiet)
    } else {
      cli::cli_inform(c(
        i = "Exiting..."
      ))
      # exits process quietly
      invokeRestart("abort")
    }
    invisible()
    
  } 
  else {
    use_schema(quiet = quiet)
  }
}

#' Find metadata info in a repository
#' @noRd
#' @keywords Internal
find_data <- function(directory,
                      quiet,
                      call = rlang::caller_env()){
  
  # determine which dwc files are present, format for message
  files <- darwin_core_files()
  user_files <- is_file_present(files, directory)
  
  if(sum(user_files$present) < 1){
    bullets <- c("No files found in {.file {directory}} to build Darwin Core Archive.",
                 i =  "Use `use_data()` and `use_metadata()` to add data and metadata files to directory.")
    cli::cli_abort(bullets,
                   call = call)
  }
  
  ## Data
  if(!quiet) {
    cli::cat_line("Data (minimum of one)")
    file_check_message(user_files, "occurrences.csv")
    file_check_message(user_files, "events.csv")
    file_check_message(user_files, "multimedia.csv")
  }
  
  # check number of files
  n_data_present <- user_files |>
    dplyr::filter(.data$type == "data") |>
    dplyr::pull("present") |>
    sum()
  
  if(n_data_present < 1){
    bullets <- c("Didn't find data files in {.file {directory}}.",
                 i = "{directory/} must contain at least one of `occurrences.csv`, `events.csv` or `multimedia.csv`.",
                 i = "See `use_data()`.")
    cli::cli_abort(bullets,
                   call = call)
  }
  
  ## Metadata
  if(!quiet){
    cli::cat_line("Metadata")
    file_check_message(user_files, "eml.xml")
  }
  
  if(!fs::file_exists(glue::glue("{directory}/eml.xml"))){
    bullets <- c("Didn't find metadata statement ({.file eml.xml}) in {.file {directory}}.",
                 i = "Create a metadata template with `use_metadata_template()`.",
                 i = "Use `use_metadata()` to convert and save a metadata statement as an {.file eml.xml} file.")
    cli::cli_abort(bullets,
                   call = call)
  }
  
  ## Schema
  if(!quiet){
    cli::cat_line("Schema")
    file_check_message(user_files, "meta.xml")
  }
  # schema does not error if missing

  # list of the files in the directory
  file_list <- user_files |>
    dplyr::filter(.data$present == TRUE) |>
    dplyr::pull("file")
  
  return(glue::glue("{directory}/{file_list}"))
}

#' Accepted file names and their types
#' @noRd
#' @keywords Internal
darwin_core_files <- function() {
  x <- tibble::tibble(
    file = c("occurrences.csv", "events.csv", "multimedia.csv",
             "eml.xml",
             "meta.xml"),
    type = c("data", "data", "data",
             "metadata",
             "schema")
  )
  
  return(x)
}

#' Find data in a repository
#' @noRd
#' @keywords Internal
is_file_present <- function(files, directory) {
  user_files <- files |>
    dplyr::mutate(
      present = glue::glue("{directory}/{files$file}") |>
        purrr::map(\(file)
                   fs::file_exists(file)) |>
        unlist())
  
  user_files <- user_files |>
    dplyr::mutate(
      present_formatted = .data$present |>
        purrr::map_chr(\(file_exists) 
                       ifelse(isTRUE(file_exists), 
                              cli::symbol$tick |> cli::col_green(), 
                              cli::symbol$cross |> cli::col_red()
                       )
        )
    )
  
  return(user_files)
}
