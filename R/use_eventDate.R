#' Add `event` data to a `tibble`
#' 
#' `eventDate` is the main variable here, and describes when an event occurred.
#' This function takes the information provided, and returns the most specific
#' information it can on when an event occurred. 
#' @param df a `data.frame` or `tibble` that the column should be appended to.
#' @param date_time an object that combines date and time information
#' @param date date only
#' @param year year only
#' @param month month only
#' @param day day only
#' @param time time of day
#' @param .keep Control which columns from .data are retained in the output. 
#' Note that unlike `dplyr::mutate`, which defaults to `"all"` this defaults to 
#' `"unused"`; i.e. only keeps Darwin Core fields, and not those fields used to 
#' generate them.
#' @returns A tibble with the requested fields.
#' @importFrom dplyr mutate
#' @importFrom rlang abort
#' @importFrom rlang warn
#' @export
use_eventDate <- function(df, 
                          date_time = NULL,
                          date = NULL,
                          year = NULL,
                          month = NULL,
                          day = NULL,
                          time = NULL,
                         .keep = "unused"
){
  df
}