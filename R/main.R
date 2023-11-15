#' Primary function to check (and optionally conform) a data frame with Darwin
#' Core standards, and return a summary report.
#'
#' Generate a report of the data columns that match Darwin Core terms,
#' summarising the compliance of the data with Darwin Core standards, and
#' recommended fixes where required. Some common issues can be corrected
#' interactively if `mend` is TRUE (experimental). Otherwise, a user can
#' manually fix their data, and run this function again to check if the data
#' conforms. against Darwin Core required and recommended (optional) fields.
#' Some common issues can be corrected when prompted.
#'
#' @param data A data frame of biological observations
#' @param mend A logical indicating whether to attempt to fix the data, FALSE by
#' default. This should be interactive (`build_` and `mend_` functions should be
#' called).
#' @param all_fields A logical indicating whether to check all fields, or only
#' required DwC fields. Default is `TRUE`.
#' @return A report of the data columns that match Darwin Core terms. If `mend`
#' is TRUE, a modified data frame will be returned depending on user input.
#'
#' @importFrom utils menu
#' @importFrom tibble tibble
#' @importFrom janitor clean_names
#' @importFrom glue glue glue_collapse
#' @importFrom readr read_csv
darwin_check <- function(data, mend, all_fields = TRUE) {
  message("
----------------------------------------
        Data Quality Check
----------------------------------------
")
  # check inputs
  if (missing(data)) {
    abort("`data` is missing, with no default")
  }

  data <- check_unique_identifiers(data)
  data <- check_fields(data, all_fields = all_fields)
  data <- check_percent_match(data)

  # check scientific names - two words (not always, subspecies), no numbers, no
  # symbols, no full stop abbreviations?
  # check_dates
  # check_places
  # check_country_codes
  # option to check against ALA name matching (search_taxa())
}

#' Building the report text
darwin_report <- function(data) {
  # Build report in pieces - one for each check function
  # Make functions return logical, and if true do nothing, false add to report
  # Section 0 - Summary (percentage match, settings used, missing vals, seed)
  # Section 1 - compliance
  # Section 2 - interactive fixes performed
  # Section 3 - further recommendations

  # Save to working directory as plain markdown
}
