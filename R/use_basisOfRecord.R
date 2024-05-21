#' Add a `basisOfRecord` field to a `tibble`
#' 
#' basisOfRecord is a standard - often required - field in the Darwin Core 
#' Standard, which describes broadly how the data were collected.
#' @param .df a `data.frame` or `tibble` that the column should be appended to.
#' @param value what value should this field take? Only accepts `camelCase`, for 
#' consistency with field names.
#' @returns A tibble with an attached `basisOfRecord` field
#' @details
#' Currently only accepts a single argument. Could probably be made more 
#' flexible.
#' @importFrom dplyr mutate
#' @importFrom rlang abort
#' @export
use_basisOfRecord <- function(.df,
                              value){
  if(missing(df)){
    abort("df is missing, with no default")
  }
  if(is.null(value)){
    abort("`value` is missing, with no default")
  }
  df |>
    mutate("basisOfRecord" = value) |>
    check_basisOfRecord(level = "abort")

}

#' Check basisOfRecord field is valid
#' @rdname check_dwc
#' @param level what action should the function take for non-conformance? 
#' Defaults to `"inform"`.
#' @order 4
#' @export
check_basisOfRecord <- function(.df, 
                                level = c("inform", "warn", "abort")
){
  level <- match.arg(level)
  if(any(colnames(.df) == "basisOfRecord")){
    .df |>
      select("basisOfRecord") |>
      check_is_string(level = level) |>
      check_contains(y = valid_basisOfRecord(), 
                     level = level)
  }
  .df
}

#' Accepted values for `BasisOfRecord`
#' @noRd
#' @keywords Internal
valid_basisOfRecord <- function(){
  c("humanObservation", 
    "machineObservation",
    "livingSpecimen",
    "preservedSpecimen",
    "fossilSpecimen",
    "materialCitation")
}