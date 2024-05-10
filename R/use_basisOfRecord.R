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


#' Check basisOfRecord field is valid
#' @rdname check_dwc
#' @param level what action should the function take for non-conformance? 
#' Defaults to `"inform"`.
#' @order 4
#' @export
check_basisOfRecord <- function(df, 
                                level = c("inform", "warn", "abort")
){
  level <- match.arg(level)
  if(any(colnames(df) == "basisOfRecord")){
    x <- df$basisOfRecord
    accepted_values <- c("humanObservation", 
                         "machineObservation",
                         "livingSpecimen",
                         "preservedSpecimen",
                         "fossilSpecimen",
                         "materialCitation")
    x_small <- unique(x) 
    x_lookup <- x_small %in% accepted_values
    if(any(!x_lookup)){
      unexpected_values <- x_small[!x_lookup]
      unexpected_string <- glue_collapse(glue("`{unexpected_values}`"),
                                         sep = ", ",
                                         last = " & ")     
      accepted_string <- glue_collapse(glue("`{accepted_values}`"),
                                       sep = ", ",
                                       last = " or ")
      bullets <- c(glue("Unexpected value(s) provided for `basisOfRecord`: {unexpected_string}"),
                   i = glue("Accepted values for `basisOfRecord` are {accepted_string}"))
      do.call(level, list(message = bullets))
    }
  }
}