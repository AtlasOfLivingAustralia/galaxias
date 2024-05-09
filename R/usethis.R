#' Add `CITATION` file to a biodiversity data repository
#' @importFrom pkgload pkg_name
#' @importFrom usethis use_directory
#' @importFrom usethis use_template
#' @export
use_bd_citation <- function(){
  use_directory("inst")
  use_template(template = "CITATION",
               save_as = "inst/CITATION",
               data = list("Project" = pkg_name()),
               package = "galaxias")
}

#' Add `data` folder to a biodiversity data repository
#' 
#' This function is largely synonymous with `usethis::use_data()`, but is 
#' included here for completeness, and to enforce some defaults that affect 
#' where and how data is stored.
#' @param ... Unquoted names of existing objects to save.
#' @param overwrite (logical) Should existing objects be overwritten? Defaults
#' to FALSE.
#' @importFrom usethis use_data
#' @export
use_bd_data <- function(..., overwrite = FALSE){
  use_data(..., 
           internal = FALSE,
           overwrite = overwrite)
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
  # update DESCRIPTION
  ## use_package("galaxias", type = "Suggests")
  # Unclear if this is needed
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
  use_directory("inst")
  build_metadata()
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

#' Add `schema.md` to a biodiversity data repository
#' 
#' Builds a file called `schema.md` in the `inst` folder; this folder is 
#' created if not already present. The schema is a standard part of Darwin Core 
#' Archives and requires data to be present in the `data` folder to do anything 
#' useful.
#' @export
use_bd_schema <- function(){
  write_md(build_schema(),
           file = "inst/schema.md")
}

#' Add Darwin Core tests to a biodiversity data repository
#' 
#' This optional first initiates the `tests/testthat` folder, then adds a set of 
#' boilerplate tests suitable for checking data. These tests all assume that 
#' data has been added to the `data` folder following the steps outlined in 
#' `data_manipulation_script.R` (created using `use_bd_data_raw()`).
#' @importFrom fs path_package
#' @importFrom usethis use_testthat
#' @export
use_bd_testthat <- function(){
  use_testthat()
  # add tests here, one at a time
  use_template(template = "test-decimalLatitude_decimalLongitude.R",
               save_as = "tests/testthat/test-decimalLatitude_decimalLongitude.R",
               package = "galaxias")
}

#' Add a vignette to report on contents of a biodiversity data repository
#' 
#' Still in test, this function builds a report-style vignette so you (and your
#' users) can see what kind of data is in your package.
#' @export
use_bd_vignette <- function(){
  use_directory("vignettes")
  use_template(template = "pkg-report.Rmd",
               save_as = "vignettes/pkg_report.Rmd",
               package = "galaxias")
}