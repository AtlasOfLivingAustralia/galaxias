#' @rdname galaxias_config
#' @order 2
#' @param x An object of class `galaxias_config`, created with 
#' `galaxias_config()`.
#' @param \dots Additional arguments, currently ignored.
#' @export
print.galaxias_config <- function(x, ...){
  str(x)
}