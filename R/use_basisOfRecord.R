#' Add a `basisOfRecord` field to a `tibble`
#' 
#' basisOfRecord is a standard - often required - field in the Darwin Core 
#' Standard, which describes broadly how the data were collected.
#' @param df a `data.frame` or `tibble` that the column should be appended to.
#' @param value what value should this field take? Only accepts `camelCase`, for 
#' consistency with field names.
#' @returns A tibble with an attached `basisOfRecord` field
#' @details
#' Currently only accepts a single argument. Could probably be made more 
#' flexible.
#' @importFrom dplyr mutate
#' @importFrom rlang abort
#' @export
use_basisOfRecord <- function(df,
                              value = c("humanObservation", 
                                        "machineObservation",
                                        "livingSpecimen",
                                        "preservedSpecimen",
                                        "fossilSpecimen",
                                        "materialCitation")){
  value <- match.arg(value)
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
    check_is_string(x)
    accepted_values <- c("humanObservation", 
                         "machineObservation",
                         "livingSpecimen",
                         "preservedSpecimen",
                         "fossilSpecimen",
                         "materialCitation")
    check_contains(unique(x), 
                   accepted_values, 
                   level = level)
  }
}