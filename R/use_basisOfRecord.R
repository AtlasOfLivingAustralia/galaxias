#' Add a `basisOfRecord` field to a `tibble`
#' 
#' basisOfRecord is a standard - often required - field in the Darwin Core 
#' Standard, which describes broadly how the data were collected.
#' @param df a `data.frame` or `tibble` that the column should be appended to.
#' @param value what value should this field take? Should be one of 
#' `"humanObservation"`, `"machineObservation"`, `"livingSpecimen"`, 
#' `"preservedSpecimen"`, `"fossilSpecimen"` or `"materialCitation"`. Only accepts 
#' camelCase, for consistency with field names.
#' @returns A tibble with an attached `basisOfRecord` field
#' @details
#' Currently only accepts a single argument. Could probably be made more 
#' flexible.
#' @importFrom dplyr mutate
#' @importFrom rlang abort
#' @export
use_basisOfRecord <- function(df,
                              value = NULL){
  
  if(missing(df)){
    abort("df is missing, with no default")
  }
  if(is.null(value)){
    abort("`value` is missing, with no default")
  }
  result <- mutate(df, basisOfRecord = value)
  check_basisOfRecord(result, level = "abort")
  result
}