#' Add CITATION file to a Biodiversity Data Package
#' @importFrom pkgload pkg_name
#' @export
use_bd_citation <- function(){
  # add `inst` with citation file
  if(!file.exists("inst")){
    dir.create("inst") 
  }
  system.file("./inst/data_package_files/CITATION", 
              package = "galaxias") |>
    readLines() |>
    gsub("`PKGNAME`", pkg_name(), x = _) |>
    writeLines(con = "inst/CITATION")
}

#' Add DESCRIPTION to a Biodiversity Data Package
#' 
#' add `usethis` to SUGGESTS (not DEPENDS, because code is only in data-raw)
#' add any packages mentioned in `data_manipulation_script.R` to SUGGESTS
#' @importFrom usethis use_description
#' @export
use_bd_description <- function(){
  use_description(fields = list(
    "Title" = "Title of this Data Package (One Line, Title Case)",
    "Description" = "Description of this data package (one paragraph).",
    "Suggests" = "dplyr,galaxias,readr,tibble,usethis"))
}

#' Add data folders to a Biodiversity Data Package
#' 
#' Add a script to `data-raw` with example code of how to rename/select/relocate 
#' fields, then save data out to `data` folder 
#' @export
use_bd_data_raw <- function(){
  if(!file.exists("data-raw")){
    dir.create("data-raw") 
  }
  system.file("./inst/data_package_files/data_manipulation_script.R", 
              package = "galaxias") |>
    file.copy(to = "data-raw") |>
    invisible()
}

#' Add a metadata statement to a Biodiversity Data Package
#' 
#' Example metadata statements can be browsed at https://collections.ala.org.au
#' @export
use_bd_metadata <- function(){
  if(!file.exists("vignettes")){
    dir.create("vignettes") 
  }
  system.file("./inst/data_package_files/vignettes/metadata_statement.Rmd", 
              package = "galaxias") |>
    file.copy(to = "vignettes") |>
    invisible()
}

#' Add README.Rmd to a Biodiversity Data Package
#' 
#' This function adds `galaxias`-specific `README.Rmd` instead of the `usethis` 
#' default. This requires replacing placeholder text with the supplied package 
#' name (`package`).
#' @importFrom pkgload pkg_name
#' @export
use_bd_readme <- function(){
  system.file("./inst/data_package_files/README.Rmd", 
              package = "galaxias") |>
    readLines() |>
    gsub("`PKGNAME`", pkg_name(), x = _) |>
    writeLines(con = "README.Rmd")
}

#' Add a schema file to a Biodiversity Data Package
#' @export
use_bd_schema <- function(){
  # this is tricky, as it needs to find stored data, load it, and map it
  # ergo it can't be run in `create_data_package()`,
  # but should be easy otherwise
  browser()
}

#' Add Darwin Core tests to a Biodiversity Data Package
#' @importFrom usethis use_testthat
#' @export
use_bd_testthat <- function(){
  use_testthat()
  system.file("./inst/data_package_files/tests",
              package = "galaxias") |>
  file.copy(to = "./tests/testthat",
            recursive = TRUE)
}