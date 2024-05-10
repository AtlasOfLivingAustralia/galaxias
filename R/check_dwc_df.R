#' Check for Darwin Core field conformance
#' 
#' Function to check whether a data.frame or tibble conforms to DwC standards
#' NOTE: Option to use `bdc` for checks, and/or `pointblank` for running them
#' @param df A tibble against which checks should be run
#' @importFrom rlang inform
#' @param df a tibble containing data
#' @order 1
#' @export
check_dwc <- function(df){
  # dwc_terms
  fields <- colnames(df)
  available_checks <- c("occurrenceID",
                        "basisOfRecord",
                        "decimalLatitude",
                        "decimalLongitude")
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
#' @order 2
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

#' @rdname check_dwc
#' @order 5
#' @export
check_decimalLatitude <- function(df, 
                                  level = c("inform", "warn", "abort")
){
  level <- match.arg(level)
  x <- df$decimalLatitude
  if(!inherits(x, "numeric")){
    inform(c(i = "`decimalLatitude` column is not numeric"))
  }else{
    if(!all(x >= -90 & x <= 90)){
      bullets <- c(i = "`decimalLatitude` column contains values outside the range `-90 <= x <= 90`")
      do.call(level, list(message = bullets))
    }
  }
}

#' @rdname check_dwc
#' @order 6
#' @export
check_decimalLongitude <- function(df, 
                                   level = c("inform", "warn", "abort")
){
  level <- match.arg(level)
  x <- df$check_decimalLongitude
  if(!inherits(x, "numeric")){
    inform(c(i = "`decimalLongitude` column is not numeric"))
  }else{
    if(!all(x >= -180 & x <= 180)){
      bullets <- c(i = "`decimalLongitude` column contains values outside the range `-180 <= x <= 180`")
      do.call(level, list(message = bullets))
    }
  }
}