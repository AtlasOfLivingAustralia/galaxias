#' Build a Darwin Core Archive from a specified directory
#' 
#' This function is similar to `write_dwca()`, but takes a directory as an 
#' argument, rather than an object of class `darwin_core_archive` from the 
#' users workspace. This allows users to build an archive without calling
#' `galaxias` within a piped workflow; but as a result, also performs less 
#' checking on the supplied objects
#' @export
build <- function(directory = "."){
  
}