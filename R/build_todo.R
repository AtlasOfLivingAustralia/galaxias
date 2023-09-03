#' Functions to build new data for compatability with DwC
#'
#' These are risky. Use with care.
#'
#' @rdname build-dwc
#' @param data A tibble
#' @param random_seed Integer: what random seed should be used? Optional, but
#' useful for reproducibility.
#' @export
build_random_identifier <- function(data,
                                    random_seed = NULL) {
  # For now a simple sequential ID?
  data$occurrenceID <- paste0("id", 1:nrow(data))
}

#' Create a composite identifier from two or more columns, separated by a colon.
#' QUESTION: Would a composite identifier column be called occurrenceID?
#' Should be globally unique - it may be necessary to still add a random number?
#' Depends on what columns are used to build it.
#' @param cols character vector of columns to use
#' @export
build_composite_identifier <- function(data,
                                       cols = NULL) {
  # check if the columns specified are contained / found in the data
  # if not, warning and check for spelling etc.
  # This method assumes string values - unchecked with other column types
  concatenated_values <- apply(data[cols], 1, function(row) {
    gsub(" ", "", tolower(paste(row, collapse = ":")))
  })
  # TODO: For now just adding a sequential numeric value
  data$occurrenceID <- paste0(concatenated_values, ":", 1:nrow(data))
  return(data)
}
cols <- c("Species", "Site")
