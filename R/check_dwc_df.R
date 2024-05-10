#' Check for Darwin Core field conformance
#' 
#' Function to check whether a data.frame or tibble conforms to DwC standards
#' NOTE: Option to use `bdc` for checks, and/or `pointblank` for running them
#' @param df A tibble against which checks should be run
#' @importFrom rlang inform
#' @export
check_dwc <- function(df){
  # dwc_terms
  fields <- colnames(df)
  available_checks <- c("basisOfRecord",
                        "decimalLatitude")
  checkable_fields <- fields[fields %in% available_checks]
  check_functions <- c("check_fields",
                       glue("check_{checkable_fields}"))
  inform("Checking DwC fields")
  # run each function on df
  lapply(check_functions, 
         function(x){do.call(x, args = list(df = df))}) |>
    invisible()
}

#' Check for non DwC fields
#' @importFrom glue glue
#' @importFrom glue glue_collapse
#' @importFrom rlang warn
#' @rdname check_dwc
#' @export
check_fields <- function(df){
  x <- colnames(df)
  if(!all(x %in% dwc_terms)){
    non_dwc <- x[!(x %in% dwc_terms)]
    if(length(non_dwc) > 1){
      dwc_string <- glue_collapse(glue("`{non_dwc}`"),
                                  sep = ", ", 
                                  last = " & ")
      inform(c(i = glue("{dwc_string} are not DwC fields")))
    }else{
      inform(c(i = glue("{non_dwc} is not a valid DwC field")))
    }
  }
}

#' Check basisOfRecord field is valid
#' @rdname check_dwc
#' @param df a tibble containing data
#' @param level what action should the function take for non-conformance? 
#' Defaults to `"inform"`.
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

#' check for decimalLatitude
#' @importFrom rlang warn
#' @export
check_decimalLatitude <- function(df){
  inform("Checking decimalLatitude")
  x <- df$decimalLatitude
  if(!inherits(x, "numeric")){
    inform(c(i = "`decimalLatitude` column is not numeric"))
  }else{
    if(!all(x >= -90 & x <= 90)){
      inform(c(i = "`decimalLatitude` column contains values outside the range `-90 <= x <= 90`"))
    }
  }
}