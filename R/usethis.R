#' Add CITATION file to a Biodiversity Data Package
#' @importFrom pkgload pkg_name
#' @importFrom usethis use_directory
#' @export
use_bd_citation <- function(){
  use_directory("inst")
  use_template(template = "CITATION",
               save_as = "inst/CITATION",
               data = list("Project" = pkg_name()),
               package = "galaxias")
}

#' Add data folders to a Biodiversity Data Package
#' 
#' Add a script to `data-raw` with example code of how to rename/select/relocate 
#' fields, then save data out to `data` folder
#' @importFrom usethis use_description
#' @importFrom usethis use_directory 
#' @importFrom usethis use_template
#' @export
use_bd_data_raw <- function(){
  use_directory("data-raw")
  use_template(template = "data_manipulation_script.R",
               save_as = "data-raw/data_manipulation_script.R",
               package = "galaxias")
  # update DESCRIPTION
  # BUT this overwrites rather than updates the file. Unclear if this is a useful approach
  # use_description(fields = list("Suggests" = "dplyr,galaxias,readr,tibble,usethis"))
}

#' Add DESCRIPTION to a Biodiversity Data Package
#' 
#' In a Biodiversity Data Package, the DESCRIPTION file is used to add 
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

#' Add README.Rmd to a Biodiversity Data Package
#' 
#' This function adds `galaxias`-specific `README.Rmd` instead of the `usethis` 
#' default. This requires replacing placeholder text with the supplied package 
#' name (`package`).
#' @importFrom usethis use_template
#' @export
use_bd_readme <- function(){
  use_template(template = "package-README",
               save_as = "README.Rmd",
               package = "galaxias")
}

#' Add Darwin Core tests to a Biodiversity Data Package
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
  # copy whole directory
  file.copy(from = path_package("galaxias", "inst/templates/testthat"),
            to = "tests",
            recursive = TRUE) |>
    invisible()
  ## alternative iterative approach
  # path <- path_package("galaxias", "inst/templates/testthat")
  # files <- list.files(path)
  # lapply(files, 
  #        function(x){use_template(template = glue("tests/{x}"),
  #                                 save_as = glue("tests/testthat/{x}"),
  #                                 package = "galaxias")     
  # }) |>
  #   invisible()
}