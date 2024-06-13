#' Check for Darwin Core field conformance
#' 
#' Function to check whether a data.frame or tibble conforms to DwC standards
#' NOTE: Option to use `bdc` for checks, and/or `pointblank` for running them
#' @param df A tibble against which checks should be run
#' @importFrom rlang inform
#' @importFrom cli cli_bullets
#' @param df a tibble containing data
#' @order 1
#' @export
check_dwc <- function(.df){
  # dwc_terms
  fields <- colnames(.df)
  available_checks <- c("occurrenceID",
                        "basisOfRecord",
                        "decimalLatitude",
                        "decimalLongitude")
  checkable_fields <- fields[fields %in% available_checks]
  check_functions <- c("check_fields",
                       glue("check_{checkable_fields}"))
  dwc_spinny_message()
  # cli_bullets(c(">" = "Checking DwC fields."))
  # run each function on df
  lapply(check_functions, 
         function(x){do.call(x, args = list(.df = .df))}) |>
    invisible()
}

#' Check for non DwC fields
#' NOTE: should probably swap to check_contains() for this.
#' @importFrom glue glue
#' @importFrom glue glue_collapse
#' @importFrom rlang warn
#' @rdname check_dwc
#' @order 2
#' @export
check_fields <- function(.df, 
                         level = c("inform", "warn", "abort")){
  level <- match.arg(level)
  result <- tibble(dwc_terms = colnames(.df)) |>
    check_contains_terms(y = dwc_terms,
                         level = level)
  .df
}

#' Wait time
#' @noRd
#' @keywords Internal
wait <- function(seconds = 1) {
  Sys.sleep(seconds)
}

#' Theatrics
#' @noRd
#' @keywords Internal
dwc_spinny_message <- function(which) {
  
  # define the spinner
  spinny <- cli::make_spinner(
    which = "dots2",
    template = "{spin} Checking DwC fields."
  )
  
  # update the spinner 100 times
  for(i in 1:100) {
    spinny$spin()
    wait(.01)
  }
  
  # clear the spinner from the status bar
  spinny$finish()
}