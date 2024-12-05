#' Create a new `galaxias` project
#' 
#' This is a wrapper to `usethis` functions `create_project()` and 
#' `create_package()`. It differs in that it builds the requisite folders
#' `"data_raw"` for 'initial' data, and `"data"` for data that has been 
#' formatted according to the Darwin Core standard.
#' @param path A path. If it exists, it is used. If it does not exist, it is 
#' created, provided that the parent path exists.
#' @param \dots Other arguments passed to `usethis::create_project()` or 
#' `usethis::create_package()`
#' @returns Does not return an object to the workspace; called for the 
#' side-effect of building a new project or package.
#' @name galaxias_project
#' @order 1
#' @importFrom paperbark use_metadata
#' @importFrom usethis create_project
#' @importFrom usethis use_directory
#' @export
galaxias_project <- function(path, ...){
  create_project(path, ...)
  use_metadata()
  use_directory("data_raw")
  use_directory("data")
  
}

#' @rdname galaxias_project
#' @order 2
#' @importFrom usethis create_package
#' @importFrom usethis use_directory
#' @export
galaxias_package <- function(path, ...){
  create_package(path, ...)
  use_directory("data_raw")
  use_directory("data") 
}
