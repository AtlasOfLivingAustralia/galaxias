#' Generate a report of the data columns that match Darwin Core terms. Will
#' attempt to conform the data to Darwin Core standards if `mend` is TRUE
#' (experimental). Otherwise, a user can manually fix their data, and run this
#' function again to check if the data conforms.
#' @param data A data frame
#' @param mend A logical indicating whether to attempt to fix the data, FALSE by
#' default. This should be interactive (`build_` and `mend_` functions should be
#' called).
#' @return A report of the data columns that match Darwin Core terms. If `mend`
#' is TRUE, a modified data frame will be returned.

darwin_check <- function(data, mend) {
  message("
----------------------------------------
        Data Quality Check
----------------------------------------
")
}



# Compliance with Darwin Core Standards:
# - scientificName: OK
# - eventDate: Inconsistent date formats found
# - basisOfRecord: OK

# Recommendations:
# - Fill in missing values in eventDate column with valid dates
# - Correct taxonomic names in the scientificName column

# ----------------------------------------
