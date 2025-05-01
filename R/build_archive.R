#' Build a Darwin Core Archive from a folder
#' 
#' @description
#' A Darwin Core archive is a zip file containing a specified combination of data
#' and metadata. `build_archive()` constructs this zip file. 
#' `build_archive()` assumes that all necessary 
#' files have been pre-constructed, and can be found inside a single folder 
#' with no additional or redundant information. 
#' 
#' `build_archive()` is similar to `devtools::build()`,
#' in the sense that it takes a repository and wraps it for publication.
#' @details
#' This function looks for three types of objects in the specified `directory`:
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
#'    Create a new template with `[use_metadata_template()]`.
#'  
#'  * Schema 
#'  
#'    A 'schema' document in xml format with the file name `meta.xml`. 
#'    This file can be constructed using [build_schema()].
#'
#' `build_archive()` will not build a Darwin Core Archive with these files 
#' present in the source directory. The resulting Archive is saved as a zip 
#' folder in the parent directory by default.
#' @param source (string) A directory containing all the files to be stored in 
#' the archive. Defaults to the `data-publish` folder within the current working 
#' directory.
#' @param destination (string) A file name to save the resulting zip file. 
#' Defaults to `./dwc-archive.zip`.
#' @return Invisibly returns the location of the built zip file; but typically
#' called for the side-effect of building a 'Darwin Core Archive' (i.e. a zip 
#' file).
#' @seealso [use_data()], [use_metadata()], [build_schema()]
#' @export
build_archive <- function(source = "data-publish", 
                          destination = "dwc-archive.zip") {
  
  cli::cli_alert_info("Building Darwin Core Archive")
  
  progress_update("Detecting files..."); wait(.1)
  
  # Check for and display which files are in source folder
  # Users will hit an error if they are 
  #  - missing all data files, or
  #  - missing an EML metadata statement
  files_in <- get_default_directory(source) |>
    find_data()
  
  # If schema file is missing, offer to build it
  if(!any(files_in %in% glue::glue("{source}/meta.xml"))){
    choice <- cli_menu(
      c(" ",
        "No schema ({.file meta.xml}) file detected.", 
        "This is a required file in a Darwin Core Archive.", 
        " "),
      "Do you want to build a schema file now? (0 to exit)",
      choices = c("Yes", "No")
    )
    
    if (choice == 1) {
      build_schema(source = source, 
                   destination = glue::glue("{source}/meta.xml"))
    } else {
      cli::cli_inform(c(
        i = "Exiting..."
      ))
      # exits process quietly
      invokeRestart("abort")
    }
    invisible()
  }
  
  progress_update("Creating zip folder...")
  file_out <- get_default_file(destination)
  browser()
  
  progress_update("Writing {.file {file_out}}.")
  zip::zip(zipfile = file_out, 
           files = files_in,
           mode = "cherry-pick")
  
  cli::cli_progress_done()
  
  invisible(file_out)
}

#' Simple function to specify a zip file if no arg given
#' @noRd
#' @keywords Internal
get_default_file <- function(file){
  if(missing(file)){
    glue::glue("{getwd()}.zip")
  }else{
    if(!grepl(".zip$", file)){
      cli::cli_abort("File must end in `.zip`.")
    }else{
      file
    }
  }
}

#' Simple function to check that a `data` directory exists if no arg given
#' @noRd
#' @keywords Internal
get_default_directory <- function(x, error_call = rlang::caller_env()){
  if(missing(x)){
    if(dir.exists("data-publish")){
      cli::cli_inform("Missing `directory`. Defaulting to {.file data-publish/} folder.")
      x <- "data-publish"
    }else{
      c("Missing `directory` and missing {.file data-publish/} folder.", 
        i = "Please specify a folder containing standardised data.") |>
      cli::cli_abort(call = error_call)
    }
  }else{
    if(!dir.exists(x)){
      c("Specified folder {.file {x}} not found.") |>
        cli::cli_abort(call = error_call)
    }else{
      x
    }
  }
}

#' Find metadata info in a repository
#' @noRd
#' @keywords Internal
find_data <- function(directory,
                      call = rlang::caller_env()){
  if(!file.exists(directory)){
    bullets <- c(glue::glue("Missing directory."),
                 i = "Use `usethis::use_data()` to add data to your project.",
                 x = "Can't find directory {.file {directory}}.")
    cli::cli_abort(bullets,
                   call = call)
  }
  
  # Acceptable Darwin Core files
  files <- tibble::tibble(
    file = c("occurrences.csv", "events.csv", "multimedia.csv",
                 "eml.xml",
                 "meta.xml"),
    type = c("data", "data", "data",
             "metadata",
             "schema")
  )
  
  # determine which files are present, format for message
  user_files <- files |>
    dplyr::mutate(
      present = glue::glue("{directory}/{files$file}") |>
        purrr::map(\(file_name)
                   file.exists(file_name)) |>
        unlist(),
      present_formatted = present |>
        purrr::map_chr(\(file_exists) 
                       ifelse(isTRUE(file_exists), 
                              cli::symbol$tick |> cli::col_green(), 
                              cli::symbol$cross |> cli::col_red()
                       )
        )
    )
  
  if(sum(user_files$present) < 1){
    bullets <- c("No files found in {.file {data-publish/}} to build Darwin Core Archive.",
                 i =  "Use `use_data()` and `use_metadata()` to add data and metadata files to directory.")
    cli::cli_abort(bullets,
                   call = call)
  }
  
  ## Data
  cli::cat_line("Data (minimum of one)")
  file_check_message(user_files, "occurrences.csv")
  file_check_message(user_files, "events.csv")
  file_check_message(user_files, "multimedia.csv")
  
  # check number of files
  n_data_present <- user_files |>
    dplyr::filter(type == "data") |>
    dplyr::pull("present") |>
    sum()
  
  if(n_data_present < 1){
    bullets <- c("Didn't find data files in {.file {directory}/}.",
                 i = "{directory/} must contain at least one of `occurrences.csv`, `events.csv` or `multimedia.csv`.",
                 i = "See `use_data()`.")
    cl::cli_abort(bullets,
                  call = call)
  }
  
  ## Metadata
  cli::cat_line("Metadata")
  file_check_message(user_files, "eml.xml")
  
  if(!file.exists(glue::glue("{directory}/eml.xml"))){
    bullets <- c("Didn't find metadata statement ({.file eml.xml}) in {.file {directory}/}.",
                 i = "Create a metadata template with `use_metadata_template()`.",
                 i = "Use `use_metadata()` to convert and save a metadata statement as an {.file eml.xml} file.")
    cli::cli_abort(bullets,
                   call = call)
  }
  
  ## Schema
  cli::cat_line("Schema")
  file_check_message(user_files, "meta.xml")
  # schema does not error if missing

  # list of the files in the directory
  file_list <- user_files |>
    dplyr::filter(present == TRUE) |>
    dplyr::pull("file")
  
  return(glue::glue("{directory}/{file_list}"))
}