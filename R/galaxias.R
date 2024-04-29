#' Use `galaxias`-specific object classes
#' 
#' The `galaxias` package uses an object-oriented framework. The `galaxias()` 
#' function is a lightweight way to access those frameworks for efficient usage.
#' The most straightforward way to achieve this is to start a pipe for building,
#' checking and publishing a Darwin Core Archive; but you can also place 
#' `galaxias()` *within* a pipe to unlock DwC-checking behaviour in associated 
#' tibbles or xml documents.
#' 
#' @name galaxias
#' @param type (string) currently only "occurrence-core" is accepted
#' @returns If no arguments are supplied, returns an object of class `dwc_a` 
#' (Darwin Core Archive). Otherwise returns `dwc_df` for a data.frame or tibble, 
#' and `dwc_xml` for xml documents.
#' @export
galaxias <- function(...){
  dots <- list(...)
  if(length(dots) < 1){
    x <- list()
    class(x) <- "dwc_a"
    x    
  }else{
    x <- dots[[1]]
    if(inherits(x, "data.frame")){
      as_dwc_df(x)
    }else if(inherits(x, "xml_document")){
      as_dwc_xml(x)
    }else{
      abort("unknown type")
    }
  }
}

#' Internal function to convert `tibble` to `dwc_df`
#' Option to add exported version using `methods::as()` later
#' @importFrom rlang abort
#' @noRd
#' @keywords Internal
as_dwc_df <- function(df){
  # checks
  if(missing(df)){
    abort("`df` is missing, with no default")
  }
  if(!inherits(df, "data.frame")){
    abort("object provided must be a `data.frame` or `tibble`")
  }
  if(!inherits(df, c("tble_df", "tbl"))){
    df <- as_tibble(df)
  }
  # convert to new class
  class(df) <- c("dwc_df", "tbl_df", "tbl", "data.frame")
  df
}

