#' Convert a `tibble` to class `dwc_df`
#' 
#' This function is experimental. It tags a tibble with an new class (`dwc_df`),
#' which enables class-specific masked functions from dplyr such as 
#' `rename.dwc()` and `mutate.dwc()` (relocate? select?).
#' 
#' It appends "dwc_df" as a class ahead of the traditional tibble classes. This
#' ensures that "dwc_df" methods are called first, but if missing, methods for
#' class "tibble" still work. This is beneficial as not all functions require
#' a dwc implementation (e.g. `filter()`).
#' @export
use_dwc <- function(df){
  class(df) <- c("dwc_df", class(df)) # "dwc_df" must go first
  df
}
# NOTE: option to make use_dwc() a generic
# could be useful to have e.g. use_dwc.data.frame vs use_dwc.xml_document
# for testing different kinds of objects

#' @rdname use_dwc
#' @export
as_tibble.dwc_df <- function(x, ...){
  class(x) <- c("tbl_df", "tbl", "data.frame")
  x
}

#' @rdname use_dwc
#' @export
print.dwc_df <- function(x, ...){
  as_tibble(x) |>
    print()
  # add messages here on DwC status if needed
}