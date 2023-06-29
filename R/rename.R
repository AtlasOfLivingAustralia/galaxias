#' Rename columns to DwC-compatible types
#' 
#' These functions are in prep
#' @param data A tibble
#' @returns A tibble
#' @rdname rename-dwc
#' @importFrom rlang inform
#' @importFrom janitor clean_names
#' @export
rename_camel_case <- function(data){
  inform("Converting column names to camel case")
  clean_names(data, case = "small_camel", abbreviations = c("ID"))
}

#' @rdname rename-dwc
#' @param .col What column should be renamed?
rename_identifier <- function(.data, .col){
  rename(data, .col = "occurrenceID")
}