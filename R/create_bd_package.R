#' Create a new Biodiversity Data Package
#' 
#' Sets up a 'Biodiversity Data Package', which is a modified R package for
#' storing biodiversity data, ideally according to the Darwin Core Standard. 
#' Based heavily on usethis::create_tidy_package
#' @param path A path to the directory to create
#' @importFrom rlang is_interactive
#' @importFrom usethis create_package
#' @importFrom usethis local_project
#' @importFrom usethis proj_activate
#' @importFrom usethis proj_get
#' @importFrom usethis use_mit_license
#' @importFrom usethis use_testthat
#' @importFrom usethis use_tidy_description
#' @export
create_bd_package <- function(path,
                              copyright_holder = NULL
                              # open = rlang::is_interactive() # not used yet
                              ){
  create_package(path, 
                 rstudio = TRUE, # argument needs to be controllable by user
                 open = FALSE)
  local_project(path)
  use_mit_license(copyright_holder)
  use_bd_readme(package = path) # ideally should detect package name without args
  use_bd_description()
  use_bd_citation(package = path) # ideally should detect package name without args
  use_bd_testthat() # `/tests`
  use_bd_data_raw() # `/data-raw`
  # note: no `use_bd_data()` as this is synonymous with `usethis::use_data()`
  use_bd_metadata() # `/vignettes`
  proj_activate(proj_get()) # launch
}

# Data packages have three levels of licencing:
# - package licence (typically MIT)
# - data licencing (recommended CC-0)
# - record or image licencing (also recommended CC-0, but may not be)
# Meaning we do not use README or DESCRIPTION for DwCA licencing,
# but instead put a different file somewhere with meta.xml and eml.xml