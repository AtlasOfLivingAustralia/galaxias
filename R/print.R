#' print functions for `galaxias` objects
#' 
#' There are three of these
#' @rdname print.dwc_a
#' @importFrom glue glue_collapse
#' @importFrom rlang inform
#' @export
print.dwc_a <- function(x, ...){
  if(length(x) < 1){
    inform("An empty Darwin Core Archive (class `dwc_a`)")
  }else{
    inform(c("A Darwin Core Archive (class `dwc_a`) containing: ", 
             glue_collapse(names(x), sep = "; ")))    
  }
}

#' @rdname print.dwc_a
#' @export
print.dwc_df <- function(x, width = NULL, ..., n = NULL){
  pillar::tbl_format_setup(x, width = width, n = n)
}

#' @rdname print.dwc_a
#' @export
print.dwc_xml <- function(x, ...){
  # unclear how this should display
  
}


# tbl_sum.dwc_df <- function(x, ...){
#   c("some text!!!")
# }