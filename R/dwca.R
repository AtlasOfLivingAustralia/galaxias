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
dwca <- function(type = "occurrence-core",
                 extentions = NULL){
  x <- vector(mode = "list", length = 3)
  names(x) <- c("data", "metadata", "column_mappings")
  class(x) <- "dwca"
  x
}

#' @rdname dwca
#' @importFrom glue glue
#' @importFrom rlang inform
#' @export
print.dwca <- function(x, ...){
  slot_text <- list()
  # add data information
  if(is.null(x$data)){
    slot_text$data <- "data: Not supplied"
  }else{
    slot_text$data <- glue("data: A tibble with {nrow(x$data)} rows and {ncol(x$data)} columns")
  }
  # metadata
  if(is.null(x$metadata)){
    slot_text$metadata <- "metadata: Not supplied"
  }else{
    slot_text$metadata <- glue("metadata: Supplied") # use xml for more info?
  }
  # column_mappings
  if(is.null(x$column_mappings)){
    slot_text$column_mappings <- "column_mappings: Not supplied"
  }else{
    slot_text$column_mappings <- glue("column_mappings: Supplied") # use tests here?
  }
  inform(c("An object of class `dwca` containing: ", 
         unlist(slot_text)))
}