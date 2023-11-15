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

# ----------------------------------------

#' User supplied column mapping to DwC terms in two column format, first column
#' user term, second column is the matching DwC term
column_matching <- function(data, mapping) {
}

# ----------------------------------------
#' ** What is this user_col_names variable - it doesn't exist anywhere
# tibble_to_dwc <- function(data) {
#   # Below code still needs reformatting to above style
#   dwc_terms <- dwc_terms_archived$column_name
#   colnames(data) <- user_col_names # q: is this sensible at this point?

#   # Identify and rename incorrectly formatted columns
#   if (any(user_col_names %in% dwc_terms) &&
#     any(user_col_names != names(data))) {
#     matched_cols <- names(data[, user_col_names %in% dwc_terms]) |>
#       sort() # TODO: Uses alphabetical order to match cols. This is hacky. Fix
#     correct_names <- dwc_terms[dwc_terms %in% user_col_names] |>
#       sort()

#     # ask if user wants to convert column names to DarwinCore case
#     rename_q_answer <-
#       menu(c("Proceed", "Exit"),
#         title = glue("
#            ---
#            Your columns are not in standard DarwinCore case format.

#            We will need reformat matched columns to DarwinCore to proceed.
#            ")
#       )
#     if (rename_q_answer == 1) {
#       bullets <- c(
#         "Column names changed:",
#         glue("{matched_cols} -> {crayon::green(correct_names)}")
#       )
#       inform(bullets)

#       data_dwc_names <- rename_columns(data, matched_cols, correct_names)
#       return(data_dwc_names)
#     } else {
#       return(data)
#     }
#   } else {
#     return(data)
#   }
# }

# #' Internal function to rename columns
# #' @noRd
# #' @keywords Internal
# #' @importFrom dplyr rename_with
# rename_columns <- function(data, matched_cols, correct_names) {
#   # rename columns
#   data_dwc_names <- data |>
#     rename_with(~ correct_names[which(matched_cols == .x)], .cols = matched_cols)

#   return(data_dwc_names)
# }
