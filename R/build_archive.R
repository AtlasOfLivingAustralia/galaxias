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
#'    altered to use Darwin Core terms as column headers. The functions
#'    `suggest_workflow()` and `check_occurrences()` may be useful here.
#'  * A metadata statement, stored in xml using the filename `eml.xml`. This can
#'    be constructed manually, or using `use_bd_metadata()` to create a markdown
#'    file followed by `build_metadata()` to save it in xml.
#'  * A 'schema' document, also stored in xml, called `meta.xml`. This is 
#'    usually constructed using `build_schema()`.
#'
#' You will get an error if these files are not present. The resulting file
#' shares the name of the working directory (with a .zip file extension),
#' and is placed in the parent directory
#' @param directory A directory containing the files to be published. Defaults
#' to the `data` folder within the current working directory.
#' @return Called exclusively for the side-effect of building a 'Darwin Core 
#' Archive' (i.e. a zip file); doesn't return anything to the workspace.
#' @importFrom glue glue
#' @importFrom zip zip
#' @export
build_archive <- function(directory = "data") {
  
  # place result next to working directory
  file_out <- glue("{getwd()}.zip")
  
  # find files in specified directory
  files_in <- find_data(directory) # also runs checks
  
  # build archive
  zip::zip(zipfile = file_out, 
           files = files_in,
           mode = "cherry-pick")
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
                 i = "use `use_bd_metadata()` then `build_metadata()` to create one")
    abort(bullets,
          call = call)
  }
  
  file_list <- c(file_list, "eml.xml", "meta.xml")
  glue("{directory}/{file_list}")
}