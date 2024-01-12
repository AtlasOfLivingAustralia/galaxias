#' Start a pipe to build a Darwin Core Archive
#' 
#' `galaxias` uses an object-oriented framework. This starts the pipe to build
#' the object in question
#' @name dwca
#' @param type (string) currently only "occurrence-core" is accepted
#' @param extentions Not currently supported; what further extentions should be added.
#' These are additional files such as 'events' or 'media' that are additional 
#' to occurrences
#' @returns An object of class `dwca` containing three slots, corresponding to 
#' the required objects for a Darwin Core Archive. 
#' @export
dwca <- function(){
  x <- list()
  class(x) <- "dwca"
  x
}

#' @rdname dwca
#' @importFrom glue glue_collapse
#' @importFrom rlang inform
#' @export
print.dwca <- function(x, ...){
  if(length(x) < 1){
    inform("An empty object of class `dwca`")
  }else{
    inform(c("An object of class `dwca` containing: ", 
             glue_collapse(names(x), sep = "; ")))    
  }
}