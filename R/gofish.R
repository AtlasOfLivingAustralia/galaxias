#' Start a pipe to build a Darwin Core Archive
#' 
#' `galaxias` uses an object-oriented framework. This starts the pipe to build
#' the object in question. `gofish()` and `darwin_core_archive()` are identical
#' in function, but not in string length.
#' @name gofish
#' @param type (string) currently only "occurrence-core" is accepted
#' @returns An object of class `darwin_core_archive`. 
#' @export
gofish <- function(){
  x <- list()
  class(x) <- "darwin_core_archive"
  x
}

#' more serious version of `gofish`
#' @rdname gofish
darwin_core_archive <- function(){gofish()}

#' @rdname gofish
#' @importFrom glue glue_collapse
#' @importFrom rlang inform
#' @export
print.darwin_core_archive <- function(x, ...){
  if(length(x) < 1){
    inform("An empty object of class `darwin_core_archive`")
  }else{
    inform(c("An object of class `darwin_core_archive` containing: ", 
             glue_collapse(names(x), sep = "; ")))    
  }
}