#' Use common Darwin Core fields
#' 
#' Shortcut methods for adding standard Darwin Core terms
#' In development, here for example purposes
#' @name use_dwc
#' @order 1
#' @export
use_occurrenceID <- function(.df,
                             ...){
  dots <- list(...)
  if(length(dots) > 0){
    .df |>
      mutate(occurrenceID = build_composite_identifier(.df, dots))
  }else{
    .df |>
      mutate(occurrenceID = build_random_identifier())
  }
}

#' Create a random identifier column
#' @param data A tibble
#' @param random_seed Integer: seed value (optional)
#' @keywords Internal
#' @noRd
build_random_identifier <- function(data,
                                    random_seed = NULL) {
  # For now just a simple sequential ID for proof of concept
  data$occurrenceID <- paste0("id", 1:nrow(data))
  # this needs to change of course. 
  # One option is uuid::UUIDgenerate(), but not sure of behaviour
  message("Random identifier column `occurrenceID` has been added to the data.\n")
  return(data)
}

#' Create a composite identifier from two or more columns, separated by a colon.
#' TODO: Question:: Would a composite identifier column be called occurrenceID?
#' Should be globally unique - it may be necessary to still add a random number?
#' Depends on what columns are used to build it.
#' @param cols character vector of columns to use
#' @keywords Internal
#' @noRd
build_composite_identifier <- function(data,
                                       cols = NULL) {
  # TODO: check if the columns specified are contained / found in the data
  # if not, warning and check for spelling etc.
  # This method assumes string values - unchecked with other column types
  concatenated_values <- apply(data[cols], 1, function(row) {
    gsub(" ", "", tolower(paste(row, collapse = ":")))
  })
  # TODO: For now just adding a sequential numeric value
  data$occurrenceID <- paste0(concatenated_values, ":", 1:nrow(data))
  return(invisible(data))
}


#' @rdname use_dwc 
#' @order 2
use_basisOfRecord <- function(.df,
                              value = c("humanObservation", 
                                        "machineObservation")){
  value <- match.arg(value)
  .df |>
    mutate(basisOfRecord = value)
}