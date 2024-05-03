#' Create a new Biodiversity Data Package
#' 
#' Sets up a 'Biodiversity Data Package', which is a modified R package for
#' storing biodiversity data, ideally according to the Darwin Core Standard. 
#' Based heavily on usethis::create_tidy_package
#' @param path A path to the directory to create
#' @importFrom usethis create_project
#' @importFrom usethis local_project
#' @importFrom usethis proj_activate
#' @importFrom usethis proj_get
#' @importFrom usethis use_directory
#' @export
create_bd_package <- function(path
                              # open = rlang::is_interactive() # not used yet
                              ){
  create_project(path, 
                 rstudio = TRUE, # argument needs to be controllable by user
                 open = FALSE)
  local_project(path, force = TRUE)
  unlink("R", recursive = TRUE) # not strictly necessary, but hightlights that this repo is for data
  use_bd_description()
  use_bd_readme()
  use_bd_data_raw() # `/data-raw`
  use_directory("data")

  ## useful, but not added for brevity reasons:
  # use_bd_citation() # optional
  # use_bd_testthat() # `/tests`
  # use_bd_metadata() # `/vignettes`
  
  proj_activate(proj_get()) # launch
}