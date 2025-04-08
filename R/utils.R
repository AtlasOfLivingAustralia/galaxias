
#' Evaluate dots
#' @keywords internal
dots <- function(...) {
  eval(substitute(alist(...)))
}

