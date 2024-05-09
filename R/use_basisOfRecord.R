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
  
  accepted_values <- c("humanObservation", 
                       "machineObservation",
                       "livingSpecimen",
                       "preservedSpecimen",
                       "fossilSpecimen",
                       "materialCitation")

  accepted_statement <- "Accepted values for `basisOfRecord` are \"humanObservation\", \"machineObservation\", `livingSpecimen\", \"preservedSpecimen\", \"fossilSpecimen\" or \"materialCitation\""
  if(is.null(value)){
    bullets <- c("`value` is missing, with no default",
                 i = accepted_statement,
                 i = "if your dataset contains different values for `basisOfRecord`, consider using `dplyr::case_when()` within a `dplyr::mutate()` call")
    abort(bullets)
  }
  
  if(!(value %in% accepted_values)){
    bullets <- c("Unexpected `value` received", 
                 i = accepted_statement)
    abort(bullets)
  }
  
  .df |>
    mutate(basisOfRecord = value)
}