#' Add `data` folder to a biodiversity data repository
#' 
#' This function places specified objects in the `data` folder as csv files.
#' Note that this is very different from `usethis::use_data` which uses `.rda`
#' format (and then only when `internal = FALSE`).
#' @param ... Unquoted names of existing objects to save.
#' @param overwrite (logical) Should existing objects be overwritten? Defaults
#' to FALSE.
#' @importFrom usethis use_data
#' @export
use_bd_data <- function(..., overwrite = FALSE){
  use_directory("data")
  write_csv(...) # pseudocode: won't work yet
}

#' Add `data-raw` folder to a biodiversity data repository
#' 
#' Add a script to `data-raw` with example code of how to rename/select/relocate 
#' fields.
#' @importFrom usethis use_directory 
#' @importFrom usethis use_template
#' @export
use_bd_data_raw <- function(){
  use_directory("data-raw")
  use_template(template = "data_manipulation_script.R",
               save_as = "data-raw/data_manipulation_script.R",
               package = "galaxias")
}

#' Add `DESCRIPTION` to a biodiversity data repository
#' 
#' In a biodiversity data repository, the DESCRIPTION file is used to add 
#' authorship and licencing information, which is then used by `build_dwca()` 
#' to create a metadata statement (in conjunction with README.Rmd)
#' @importFrom usethis use_description
#' @export
use_bd_description <- function(){
  use_description(fields = list(
    "Title" = "Title of this Data Package (One Line, Title Case)",
    "Description" = "Description of this data package (one paragraph).",
    "Licence" = "`use_ccby_licence()` (recommended), `use_cc0_licence()` or friends to pick a licence appropriate for a data package"))
}

#' Add a metadata statement to a biodiversity data repository
#' 
#' Builds a file called `metadata.md` in the `inst` folder; this folder is 
#' created if not already present. Partially populated using `DESCRIPTION`.
#' @importFrom usethis use_directory
#' @export
use_bd_metadata <- function(){
  use_template(template = "pkg-metadata",
               save_as = "metadata.md",
               package = "galaxias")
}

#' Add `README` to a biodiversity data repository
#' 
#' This function adds `galaxias`-specific `README` instead of the `usethis` 
#' default. Note that the two functions deliver quite different content. 
#' `README.md` is intended as a metadata statement, for *projects* (i.e. same
#' as `use_bd_metadata()` for packages, but in a different location). 
#' `README.Rmd` is for describing *packages* and does not have a metadata-like
#' flavour.
#' @name use_bd_readme
#' @importFrom usethis use_template
#' @export
use_bd_readme_md <- function(){
  use_template(template = "project-README",
               save_as = "README.md",
               package = "galaxias")
}

#' @rdname use_bd_readme
#' @importFrom usethis use_template
#' @export
use_bd_readme_rmd <- function(){
  use_template(template = "pkg-README",
               save_as = "README.Rmd",
               data = list("Project" = pkg_name()),
               package = "galaxias")
}