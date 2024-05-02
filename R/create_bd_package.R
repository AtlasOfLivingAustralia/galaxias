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
#' @export
create_bd_package <- function(path
                              # open = rlang::is_interactive() # not used yet
                              ){
  create_project(path, 
                 rstudio = TRUE, # argument needs to be controllable by user
                 open = FALSE)
  local_project(path, force = TRUE)
  use_bd_description()
  use_bd_readme()
  use_bd_testthat() # `/tests`
  use_bd_data_raw() # `/data-raw`
  use_bd_metadata() # `/vignettes`
  ## not called here:
  # use_bd_citation() # optional
  # use_schema() # requires user to add data before this does anything useful
  # use_bd_data() # not written, as synonymous with `usethis::use_data()`
  proj_activate(proj_get()) # launch
}

# Data packages have three levels of licencing:
# - package licence (typically MIT)
# - data licencing (recommended CC-0)
# - record or image licencing (also recommended CC-0, but may not be)
# Meaning we do not use README or DESCRIPTION for DwCA licencing,
# but instead put a different file somewhere with meta.xml and eml.xml