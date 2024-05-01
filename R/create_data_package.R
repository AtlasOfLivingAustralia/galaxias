#' Function to create a new RStudio Project
#' 
#' Sets up all 'Biodiversity Data Package', which is a modified R package for
#' building Darwin Core Archives. Based heavily on usethis::create_tidy_package
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
create_data_package <- function(path, 
                                copyright_holder = NULL
                                # open = rlang::is_interactive() # not used yet
                                ){
  create_package(path, rstudio = TRUE, open = FALSE)
  local_project(path)
  use_mit_license(copyright_holder)
  use_data_readme(package = path) # ideally should detect package name without args
  use_data_description()
  use_data_citation(package = path) # ideally should detect package name without args
  use_data_tests() # `/tests`
  use_data_examples() # `/data-raw`, `/data`
  use_data_metadata() # `/vignettes`
  proj_activate(proj_get()) # Launch
}

# Data packages have three levels of licencing:
# - package licence (typically MIT)
# - data licencing (recommended CC-0)
# - record or image licencing (also recommended CC-0, but may not be)
# Meaning we do not use README or DESCRIPTION for DwCA licencing,
# but instead put a different file somewhere with meta.xml and eml.xml