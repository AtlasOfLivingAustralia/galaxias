#' Create a new Biodiversity Data Project
#' 
#' Sets up a 'Biodiversity Data Project', which is a repository for
#' storing biodiversity data, ideally according to the Darwin Core Standard. 
#' In practice, this is a modified version of `usethis::create_project()`, 
#' and calls that function internally.
#' @param path A path to the directory to create
#' @param rstudio (logical) should we create an RStudio project?
#' @param open (logical) should the resulting project be opened?
#' @importFrom rlang is_interactive
#' @importFrom rstudioapi isAvailable
#' @importFrom usethis create_project
#' @importFrom usethis local_project
#' @importFrom usethis proj_activate
#' @importFrom usethis proj_get
#' @importFrom usethis use_directory
#' @importFrom withr deferred_clear
#' @export
create_bd_project <- function(path,
                              rstudio = rstudioapi::isAvailable(),
                              open = rlang::is_interactive()
                              ){
  create_project(path, 
                 rstudio = rstudio,
                 open = FALSE)
  local_project(path, force = TRUE)
  unlink("R", recursive = TRUE)
  use_bd_readme_md()
  use_bd_data_raw()
  use_directory("data")
  if (open) {
    if (proj_activate(proj_get())) {
      withr::deferred_clear()
    }
  }
  invisible(proj_get())
}