#' Build a Darwin Core Archive from a Biodiversity Data Package
#' 
#' Similar to `devtools::build()`, but to construct a Darwin Core Archive
#' rather than an R package.
#' @details
#' This function looks for specific objects in specific places, namely:
#' 
#'  * Named objects in the `data` folder
#'  * `metadata.md` in base folder (or `inst` folder as a backup)
#' 
#' If either data or metadata are missing missing, the function will
#' terminate with an error. 
#' 
#' Note that Darwin Core Archives also require a 'schema' file. This is 
#' buily internally using `build_schema()`.
#' @param project A directory containing a Biodiversity Data Project. Defaults
#' to current working directory.
#' @param path (Optional) Path to the resulting zip file. Defaults to `NULL`, 
#' indicating that the file should be saved to the current parent directory.
#' @return Called exclusively for the side-effect of building a 'Darwin Core 
#' Archive' (i.e. a zip file); doesn't return anything to the workspace.
#' @importFrom glue glue
#' @importFrom readr write_csv
#' @importFrom usethis local_project
#' @importFrom xml2 write_xml
#' @importFrom zip zip
#' @export
build_dwca <- function(project = ".",
                       path = NULL) {
  # set working directory to specified location
  local_project(project)
  
  # safely build temporary folder, return path as a string
  project_path <- get_temp_path()
  
  # get paths to data files (note: errors if missing)
  metadata_path <- find_metadata()
  data_path <- find_data()
  
  # create `eml.xml` from `metadata.md`
  create_dwc_metadata(file = metadata_path,
                      target = project_path)
  
  # create data from `data_path`
  transfer_dwc_data(files = data_path,
                    target = project_path)
  
  # add schema
  build_schema(project_path) |>
    write_xml(file = glue("{project_path}/meta.xml"))
  
  # save as zip file in requested location
  if(is.null(path)){
    path <- getwd() |>
      sub("/([[:alnum:]]|-|_)+$", "", x = _)    
  }
  target_file <- glue("{path}/{get_proj_name()}.zip")
  zip::zip(zipfile = target_file, 
           files = list.files(project_path),
           mode = "cherry-pick")
  unlink(working_file)
}


#' Internal function to get project name
#' Differs from pkgload::pkg_name in not using DESCRIPTION
#' @noRd
#' @keywords Internal
get_proj_name <- function(){
  ## option to use file path to get directory name:
  file_path <- getwd()
  sub(glue("^{dirname(file_path)}/"), "", file_path)
  ## option to use .Rproj file name - ignored for now as more easily corrupted
  # file_name <- list.files(pattern = ".Rproj$")
  # sub(".Rproj$", "", file_name)
}


#' Safely make a temporary directory, and return its' location
#' @noRd
#' @keywords Internal
get_temp_path <- function(){
  project_name <- get_proj_name()
  temp_loc <- tempdir() # create file structure in temporary directory
  project_path <- glue("{temp_loc}/{project_name}")
  # remove previous builds, if present
  if(file.exists(project_path)){
    unlink(project_path, recursive = TRUE)
  }
  dir.create(project_path)
  project_path
}

#' Find metadata info in a repository
#' @importFrom rlang abort
#' @noRd
#' @keywords Internal
find_data <- function(){
  if(!file.exists("data")){
    bullets <- c("`data` directory is required, but missing",
                 i = "use `usethis::use_data()` to add data to your package")
    abort()
  }
  accepted_names <- c("occurrences", "events", "media") |>
    glue_collapse(sep = "|")
  accepted_file_types <- c(".csv", ".rda") |>
    glue_collapse(sep = "|")
  accepted_string <- glue("^({accepted_names})({accepted_file_types})$")
  file_list <- list.files("data",
                          pattern = accepted_string)
  if(length(file_list) < 1){
    bullets <- c("No data meeting Darwin Core requirements is given in `data`.",
                 i = "use `add_bd_data_raw()` for examples of how to add raw data to your package",
                 i =  "use `usethis::use_data()` to add data to your package")
    abort(bullets)
  }else{
    glue("./data/{file_list}")
  }
}

#' Find metadata info in a repository
#' @importFrom rlang abort
#' @noRd
#' @keywords Internal
find_metadata <- function(){
  if(!file.exists("metadata.md")){
    bullets <- c("`metadata.md` is required, but missing from the specified folder",
                 i = "use `use_bd_metadata()` to create a boilerplate metadata statement")
    abort(bullets)
  }else{
    "metadata.md"
  }
  # option to search for other suffixes (.Rmd, .Qmd) here
}

#' function to take files from `data` and move them to DWCA
#' @importFrom rlang inform
#' @noRd
#' @keywords Internal
transfer_dwc_data <- function(files, target){
  if(length(files) > 0){
    files_short <- sub("^./data/", "", files)
    target_files <- glue("{target}/{files_short}")
    for(i in seq_along(files)){
      file.copy(from = files[i], to = target_files[i]) # purrr::walk?
    }
  }else{
    inform("no files found")
  }
}


#' import metadata from a file, and convert to xml in a DWCA
#' @importFrom xml2 write_xml
#' @noRd
#' @keywords Internal
create_dwc_metadata <- function(file, target){
  read_md(file) |>
    write_xml(file = glue("{target}/eml.xml"))
}