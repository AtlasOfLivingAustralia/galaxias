#' Functions to build new data for compatability with DwC
#' 
#' These are risky. Use with care
#' @rdname build-dwc
#' @param data A tibble
#' @param random_seed Integer: what random seed should be used? Optional, but 
#' useful for reproducibility.
#' @export
build_random_identifier <- function(data,
                                    random_seed = NULL
                                    ){
  data
}

#' @rdname build-dwc
#' @param .cols If `type = "merge"`, what columns should be used?
#' @export
build_composite_identifier <- function(data, 
                                       .cols = NULL){
  data
}