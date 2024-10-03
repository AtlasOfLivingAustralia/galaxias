#' Build a Darwin Core Archive from a folder
#' 
#' A Darwin Core archive is a zip file with a specified combination of data
#' and metadata. This function assumes that all of these file types have been
#' pre-constructed, and can be found inside a single folder, with no additional
#' or redundant information. This function is similar to `devtools::build()`,
#' in the sense that it takes a repository and wraps it for publication, without
#' assessing the contents in any meaningful way. It differs from 
#' `devtools::build()` in that it builds a Darwin Core Archive, rather than an 
#' R package.
#' @details
#' This function looks for three types of objects in the specified `directory`:
#' 
#'  * One or more `csv` files such as `occurrences.csv` &/or `events.csv`. 
#'    These will be manipulated versions of the raw dataset, which have been
#'    altered to use Darwin Core terms as column headers. See the `corroboree`
#'    package for details.
#'  * A metadata statement, stored in xml using the filename `eml.xml`. The
#'    function `use_metadata()` from the `elm` package is a good starting point
#'    here, followed by `build_metadata()` to save it in xml.
#'  * A 'schema' document, also stored in xml, called `meta.xml`. This is 
#'    usually constructed using `build_schema()`.
#'
#' You will get an error if these files are not present. The resulting file
#' shares the name of the working directory (with a .zip file extension),
#' and is placed in the parent directory
#' @param x (string) A directory containing all the files to be stored in the
#' archive. Defaults to the `data` folder within the current working directory.
#' @param file (string) A file name to save the resulting zip file.
#' @return Invisibly returns the location of the built zip file; but typically
#' called for the side-effect of building a 'Darwin Core Archive' (i.e. a zip 
#' file).
#' @importFrom zip zip
#' @export
build_archive <- function(x = "data", file) {
  x <- get_default_directory(x)
  files_in <- find_data(x)
  file_out <- get_default_file(file)
  zip::zip(zipfile = file_out, 
           files = files_in,
           mode = "cherry-pick")
  invisible(return(file_out))
}

#' Simple function to specify a zip file if no arg given
#' @importFrom glue glue
#' @importFrom rlang abort
#' @noRd
#' @keywords Internal
get_default_file <- function(file){
  if(missing(file)){
    glue("{getwd()}.zip")
  }else{
    if(!grepl(".zip$", file)){
      abort("file must end in `.zip`")
    }else{
      file
    }
  }
}

#' Simple function to check that a `data` directory exists if no arg given
#' @importFrom rlang abort
#' @importFrom rlang inform
#' @importFrom glue glue
#' @noRd
#' @keywords Internal
get_default_directory <- function(x){
  if(missing(x)){
    if(dir.exists("data")){
      inform("`x` is missing; defaulting to `data` folder")
      x <- "data"
    }else{
      abort(c("`x` is missing, and `data` folder is missing", 
              i = "please supply a folder containing required data"))
    }
  }else{
    if(!dir.exists(x)){
      abort(glue("Specified folder '{x}' not found"))
    }else{
      x
    }
  }
}

#' Find metadata info in a repository
#' @importFrom glue glue_collapse
#' @importFrom rlang abort
#' @importFrom rlang caller_env
#' @noRd
#' @keywords Internal
find_data <- function(directory,
                      call = caller_env()){
  if(!file.exists(directory)){
    bullets <- c(glue("`{directory}` directory is required, but missing."),
                 i = "use `usethis::use_data()` to add data to your project.")
    abort(bullets,
          call = call)
  }
  accepted_names <- c("occurrences", 
                      "events", 
                      "multimedia") |>
    glue_collapse(sep = "|")
  file_list <- list.files(directory,
                          pattern = glue("^{accepted_names}.csv$"))
  if(length(file_list) < 1){
    bullets <- c("No data meeting Darwin Core requirements is given in `data`.",
                 i = "use `add_bd_data_raw()` for examples of how to add raw data to your package",
                 i =  "use `usethis::use_data()` to add data to your package")
    abort(bullets,
          call = call)
  }
  
  if(!file.exists(glue("{directory}/meta.xml"))){
    bullets <- c("No schema file (`meta.xml`) is present in the specified directory.",
                 i = "use `build_schema()` to create one")
    abort(bullets,
          call = call)
  }
  
  if(!file.exists(glue("{directory}/eml.xml"))){
    bullets <- c("No metadata statement (`eml.xml`) is present in the specified directory.",
                 i = "See `elm::use_metadata()` for an example metadata statement,",
                 i = "then `build_metadata()` to convert to `eml.xml`.")
    abort(bullets,
          call = call)
  }
  
  file_list <- c(file_list, "eml.xml", "meta.xml")
  glue("{directory}/{file_list}")
}