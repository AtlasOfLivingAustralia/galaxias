# The `check_data_field.R` functions are used to assess user data for
# compatibility with DwC format. Specifically, these checks pertain to detecting
# the presence or absence of required and recommended fields. Besides detecting
# if any column contains unique values, these functions to not assess the
# content of the data within each column. For this, see `check_data_values.R`.

#' `Check_fields()` wrapper function that calls all other `check_*()` functions
#' @param data A tibble
#' @param all_fields A logical indicating whether to check all fields, or only
#' required fields. Default is `TRUE`.
check_fields <- function(data, all_fields = TRUE) {
  if (all_fields) {
    data <- check_unique_identifiers(data)
    check_required_fields(data)
    check_recommended_fields(data)
  } else {
    data <- check_unique_identifiers(data)
    check_required_fields(data)
  }
  check_percent_match(data)
  # return invisible so that the data output is not printed to the console,
  # which blocks the view of the console reporting
  return(invisible(data))
}

#' Check for presence of only the required fields in DwC occurrence ID is also
#' required but this is checked in `check_unique_identifiers()`
#' @importFrom rlang abort
check_required_fields <- function(data) {
  message("\nChecking fields required by Darwin Core...")
  message("---------------------------------------------\n")
  column_names <- colnames(data)
  required_cols <- c("scientificName", "eventDate", "basisOfRecord")
  missing_required_fields <- !(required_cols %in% column_names)
  if (any(missing_required_fields)) {
    unmatched <- required_cols[missing_required_fields]
    list_of_unmatched <- glue::glue_collapse(unmatched,
      sep = "\n - "
    )
    bullets <- c(
      "Missing required columns.\n",
      i = glue::glue("Darwin Core standards require that `scientificName`,`eventDate` & `basisOfRecord` columns are supplied.\n\n"),
      x = glue::glue("\nMissing required column(s):\n - {list_of_unmatched}")
    )
    message(bullets)
  } else {
    message("All required fields present...")
  }
  return(invisible(NULL))
}

#' Check for presence of only the recommended fields in DwC
#' @importFrom rlang inform
check_recommended_fields <- function(data) {
  message("\nChecking fields recommended by Darwin Core...")
  message("---------------------------------------------\n")
  column_names <- colnames(data)
  recommended_cols <- c(
    "kingdom", "taxonRank",
    "decimalLatitude", "decimalLongitude", "geodeticDatum",
    "countryCode",
    "individualCount", "organismQuantity", "organismQuantityType"
  )
  missing_recommended_fields <- !(recommended_cols %in% column_names)
  if (any(missing_recommended_fields)) {
    unmatched <- recommended_cols[missing_recommended_fields]
    list_of_unmatched <- glue::glue_collapse(unmatched,
      sep = "\n - "
    )
    bullets <- c(
      "Missing recommended columns.\n",
      i = glue::glue("It is recommended to include as many of the Darwin Core recommended columns as you can.\n\n"),
      x = glue::glue("\nMissing recommended column(s):\n - {list_of_unmatched}")
    )
    message(bullets)
  } else {
    message("All recommended fields present...")
  }
  return(invisible(NULL))
}

#' Check for presence of a unique identifier field required by DwC
#'
#' The `check_unique_identifiers()` function is used to check for the presence
#' of any of the three unique identifier fields required by DwC (`occurrenceID`,
#' `catalogueNumber`, `recordNumber`). If none are found, the function will call
#' `detect_unique_column()`, which will attempt to detect a unique column in the
#' data that may be renamed, or else assist the user in creating one.
#' @param data A tibble
check_unique_identifiers <- function(data) {
  message("Checking for unique identifier column...\n")
  column_names <- colnames(data)
  required_columns <- c("occurrenceID", "catalogueNumber", "recordNumber")
  if (!any(column_names %in% required_columns)) {
    message(
      "Missing identifier column:\n",
      "Darwin Core standards require that one of\n - `occurrenceID`\n - `catalogueNumber`\n - `recordNumber`\ncolumns are supplied."
    )
    # attempt to detect unique column
    message("Checking if a unique-like column is present...")
    detect_unique_column(data)
  } else {
    message("Unique identifier column detected, continuing...\n")
    return(invisible(NULL))
  }
}

# TODO: the check will perform at the end, using the data that has by now been
# potentially renamed / updated etc. However, a user may also like to use stand
# alone before running the complete check, to get a quick overview.
# TODO: The dwc_terms_archived is all possible terms or only required? Should
# have two versions one for required and one for required and recommended.
check_percent_match <- function(data) {
  message("\nChecking total percent match to Darwin Core terms...")
  message("---------------------------------------------\n")
  column_names <- colnames(data)
  dwc_terms <- dwc_terms_archived$column_name
  user_total_cols <- length(column_names)

  # Check for complete match
  if (all(column_names %in% dwc_terms)) {
    inform(glue::glue("{crayon::green('100% of columns match DarwinCore terms')}"))
  } else {
    n_matched <- sum(column_names %in% dwc_terms)
    prop_matched <- paste(round(n_matched / user_total_cols * 100, 1),
      "%",
      sep = ""
    )
    unmatched <- column_names[!column_names %in% dwc_terms]
    unmatched_names_list <- glue_collapse(unmatched,
      sep = ", ",
      last = " and "
    )

    bullets <- c(
      glue("{crayon::red(prop_matched)} {crayon::red('of columns match DarwinCore terms')}"),
      x = glue("Unmatched columns: {unmatched_names_list}")
    )
    inform(bullets)
  }

  return(NULL)
}
