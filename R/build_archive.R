#' Build a Darwin Core Archive from a folder
#' 
#' @description
#' A Darwin Core archive is a zip file with a specified combination of data
#' and metadata. This function assumes that all of these file types have been
#' pre-constructed, and can be found inside a single folder, with no additional
#' or redundant information. This function is similar to `devtools::build()`,
#' in the sense that it takes a repository and wraps it for publication, It 
#' differs from `devtools::build()` in that it builds a Darwin Core Archive, 
#' rather than an R package.
#' @details
#' This function looks for three types of objects in the specified `directory`:
#' 
#'  * One or more `csv` files such as `occurrences.csv` &/or `events.csv`. 
#'    These will be manipulated versions of the raw dataset, which have been
#'    altered to use Darwin Core terms as column headers. See 
#'    [corella::corella-package()] for details.
#'  * A metadata statement, stored in `EML` using the filename `eml.xml`. The
#'    function [use_metadata_template()] is a good starting point here, followed by 
#'    [use_metadata()] once you have populated your metadata statement.
#'  * A 'schema' document, also stored in xml, called `meta.xml`. This is 
#'    usually constructed using [build_schema()].
#'
#' You will get an error if these files are not present. The resulting file
#' shares the name of the working directory (with a .zip file extension),
#' and is placed in the parent directory.
#' @param source (string) A directory containing all the files to be stored in 
#' the archive. Defaults to the `data-publish` folder within the current working 
#' directory.
#' @param destination (string) A file name to save the resulting zip file.
#' @return Invisibly returns the location of the built zip file; but typically
#' called for the side-effect of building a 'Darwin Core Archive' (i.e. a zip 
#' file).
#' @export
build_archive <- function(source = "data-publish", destination) {
  progress_update("Retrieving data...")
  files_in <- get_default_directory(source) |>
    find_data()
  
  progress_update("Creating zip folder...")
  file_out <- get_default_file(destination)
  
  progress_update("Building Darwin Core Archive...")
  zip::zip(zipfile = file_out, 
           files = files_in,
           mode = "cherry-pick")
  
  cli::cli_alert_success("Darwin Core Archive successfully built. \nSaved as {.file {file_out}}.")
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
    bullets <- c(glue::glue("Missing `directory`."),
                 i = "Use `usethis::use_data()` to add data to your project.",
                 x = "Can't find directory {.file {directory}}.")
    cli::cli_abort(bullets,
                   call = call)
  }
  accepted_names <- c("occurrences", 
                      "events", 
                      "multimedia") |>
    glue::glue_collapse(sep = "|")
  file_list <- list.files(directory,
                          pattern = glue::glue("^{accepted_names}.csv$"))
  if(length(file_list) < 1){
    bullets <- c("Can't find data meeting Darwin Core requirements in {.file {data-publish/}}.",
                 i = "Use `add_bd_data_raw()` for examples of how to add raw data to your package.",
                 i =  "Use `usethis::use_data()` to add standardised data to {.file {data-publish/}}.")
    cli::cli_abort(bullets,
                   call = call)
  }
  
  ## Message about what data files galaxias detected?
  
  if(!file.exists(glue("{directory}/meta.xml"))){
    bullets <- c("No schema file ({.file meta.xml}) is present in the specified directory.",
                 i = "Use `build_schema()` to create a schema file.")
    cl::cli_abort(bullets,
                  call = call)
  }
  
  # Message about finding the schema file and its name?
  
  if(!file.exists(glue("{directory}/eml.xml"))){
    bullets <- c("No metadata statement ({.file eml.xml}) is present in the specified directory.",
                 i = "See `delma::use_metadata_template()` for an example metadata statement.",
                 i = "Use `use_metadata()` to convert to {.file eml.xml}.")
    cli::cli_abort(bullets,
                   call = call)
  }
  
  # Message about finding the file and its name?
  
  file_list <- c(file_list, "eml.xml", "meta.xml")
  glue::glue("{directory}/{file_list}")
}