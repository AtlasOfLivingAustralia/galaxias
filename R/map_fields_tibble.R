#' Tools for mapping columns to DwC fields
#'
#' @description
#' Imports a mapping table, ensuring dataset column names align with DwC terms.
#'
#' * Requires a data frame with at least two columns:
#'   - First: Column names of your dataset.
#'   - Second: Corresponding DwC terms.
#' * Each row must correctly pair a dataset column name with a DwC term, e.g.,
#'   `data.frame("sci name", "scientificName")`.
#' @section dev notes:
#' In progress, potential use cases:
#' * meta xml mapping process
#' * E2E use; pipeline w/ existing lookup table
#' * Importing a lookup table created in a previous session (with `map2dwc()`)
#' @param mapping data frame Containing at least two columns
#' @param index numeric vector of length 2. The first element is the column
#'   index to use for the data column names, the second element is the column
#'   index to use for the DwC terms e.g. `index = c(1, 2)`.
#' @param dataset data frame Optional dataset to which the lookup table
#'   pertains, used for validation.
#' @rdname map_fields
#' @export
map_fields_tibble <- function(mapping, index, dataset = NULL) {
  # Check that index is numeric length 2
  if (!is.numeric(index) || length(index) != 2) {
    stop(cli::format_error(c(" ",
                             "x" = "Index must be a numeric vector of length 2"
    )))
  }
  # Check that the index is valid for the data frame
  if (any(index > ncol(data))) {
    stop(cli::format_error(c(" ",
                             "x" = "Index contains values > than number of columns"
    )))
  }
  # UNTESTED Check if all terms are valid
  if (!all(mapping[, index[2]] %in% dwc_terms_archived$column_name)) {
    stop(cli::format_error(c(" ",
                             "x" = "Invalid DwC terms detected"
    )))
  }
  # UNTESTED Check column names in lookup table are present in dataset
  if (!is.null(dataset)) {
    if (!all(mapping[, index[1]] %in% colnames(dataset))) {
      stop(cli::format_error(c(" ",
                               "x" = "Column names in lookup table not present in dataset"
      )))
    }
  }
}