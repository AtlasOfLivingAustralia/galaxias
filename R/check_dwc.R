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