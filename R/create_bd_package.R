#' Create a new Biodiversity Data Package
#' 
#' Sets up a 'Biodiversity Data Package', which is a modified R package for
#' storing biodiversity data, ideally according to the Darwin Core Standard. 
#' In practice, this is a modified version of `usethis::create_package()`.
#' @param path A path to the directory to create
#' @param rstudio (logical) should we create an RStudio project?
#' @param open (logical) should the resulting project be opened?
#' @importFrom rlang is_interactive
#' @importFrom rstudioapi isAvailable
#' @importFrom usethis create_project
#' @importFrom usethis local_project
#' @importFrom usethis proj_activate
#' @importFrom usethis proj_get
#' @importFrom usethis use_build_ignore
#' @importFrom usethis use_ccby_license
#' @importFrom usethis use_directory
#' @importFrom withr deferred_clear
#' @export
create_bd_package <- function(path,
                              rstudio = rstudioapi::isAvailable(),
                              open = rlang::is_interactive()
                              ){
  create_project(path, 
                 rstudio = rstudio,
                 open = FALSE)
  local_project(path, force = TRUE)
  use_bd_description()
  use_namespace()
  use_bd_readme_rmd()
  use_bd_data_raw() # `/data-raw`
  use_ccby_license()
  use_bd_metadata()
  use_build_ignore("metadata.md")
  use_directory("data")
  use_directory("R")
  use_bd_testthat()
  if (open) {
    if (proj_activate(proj_get())) {
      withr::deferred_clear()
    }
  }
  invisible(proj_get())
}