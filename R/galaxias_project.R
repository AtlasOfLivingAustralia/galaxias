#' Create a new `galaxias` project
#' 
#' This is a wrapper to `usethis` functions `create_project()` and 
#' `create_package()`. It differs in that it builds the requisite folders
#' `"data-raw"` for 'initial' data, `"data"` for processed data, and 
#' `"data-published"` for data that has been 
#' formatted according to the Darwin Core Standard.
#' @param path A path. If it exists, it is used. If it does not exist, it is 
#' created, provided that the parent path exists.
#' @param \dots Other arguments passed to `usethis::create_project()` or 
#' `usethis::create_package()`
#' @returns Does not return an object to the workspace; called for the 
#' side-effect of building a new project or package.
#' @name galaxias_project
#' @order 1
#' @export
galaxias_project <- function(path, ...){
  usethis::create_project(path, ...)
  delma::use_metadata_template("metadata.Rmd")
  usethis::use_directory("data-raw")
  usethis::use_directory("data")
  usethis::use_directory("data-published")
}

#' @rdname galaxias_project
#' @order 2
#' @export
galaxias_package <- function(path, ...){
  usethis::create_package(path, ...)
  usethis::use_directory("data-raw")
  usethis::use_directory("data") 
  usethis::use_directory("data-published") 
}
