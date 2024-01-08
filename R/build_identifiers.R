#' Create a random identifier column
#' @name build_identifiers
#' @order 1
#' @param data A tibble
#' @param random_seed Integer: seed value (optional)
build_random_identifier <- function(data,
                                    random_seed = NULL) {
  # For now just a simple sequential ID for proof of concept
  data$occurrenceID <- paste0("id", 1:nrow(data))
  message("Random identifier column `occurrenceID` has been added to the data.\n")
  return(data)
}

#' Create a composite identifier from two or more columns, separated by a colon.
#' TODO: Question:: Would a composite identifier column be called occurrenceID?
#' Should be globally unique - it may be necessary to still add a random number?
#' Depends on what columns are used to build it.
#' @param cols character vector of columns to use
#' @rdname build_identifiers
#' @order 2
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