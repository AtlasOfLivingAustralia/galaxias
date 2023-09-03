#' check_* functions to assess user data for compatibility with DwC format.
#'

#' Check_unique_identifiers will check for a unique identifier column, required
#' by DwC.
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
    return(data)
  }
}

#' Detect if a unique-like column is found in the data
#'
#' Unique like columns are detected (every row is unique). If yes, the user is
#' given the option to rename it. If no unique column is detected, the user is
#' given the option to create one.
#' to comply with DwC standard.
detect_unique_column <- function(data) {
  result <- data %>%
    dplyr::summarise(dplyr::across(
      dplyr::everything(),
      ~ dplyr::n_distinct(.) == nrow(data)
    ))
  if (any(result)) {
    unique_columns <- names(result)[unlist(result)]
    sprintf("Column(s) detected with unique values: %s", unique_columns)
    message("Would you like to rename detected column(s) to comply with Darwin Core standards?")
    # Add a yes or no prompt here
    # if yes, rename interactive
    # else, return to check sequence
    rename_interactive(data, unique_columns)
  } else {
    message("No unique columns detected. Would you like to create one?")
    # if yes:
    # build_random_identifier()
    # build_composite_identifier()
    # else, return to check sequence
  }
}

#' Rename a column interactively.
#'
#' This is called by `detect_unique_column` when one or more unique columns are
#' detected. The user is then given the option to rename the column to one of
#' the Darwin Core standard identifiers, if it is appropriate to do so.
rename_interactive <- function(data, unique_columns) {
  for (col in unique_columns) {
    cat("Column:", col, "\n")
    cat("Current name:", col, "\n")
    cat("Choose an action:\n")
    cat("1. Rename to 'occurrenceID'\n")
    cat("2. Rename to 'catalogueNumber'\n")
    cat("3. Rename to 'recordNumber'\n")
    cat("4. Skip\n")

    choice <- as.integer(readline("Enter your choice: "))

    if (choice == 1) {
      new_name <- "occurrenceID"
    } else if (choice == 2) {
      new_name <- "catalogueNumber"
    } else if (choice == 3) {
      new_name <- "recordNumber"
    } else if (choice == 4) {
      new_name <- NULL
    } else {
      cat("Invalid choice. Skipping column.\n")
      new_name <- NULL
    }

    if (!is.null(new_name)) {
      message("Warning: column X of your data will be renamed to Y.
       are you sure you want to continue?")
      # Insert a yes no prompt here
      data <- data %>% dplyr::rename(!!new_name := {{ col }})
    }
    cat("\n")
  }
  # Message: X columns renamed successfully
  return(data)
}

#' Check fields
#'
#' Check fields required and recommended by Darwin Core standards.
#' @param data A tibble
#' @param all_fields A logical indicating whether to check all fields, or only
#' required fields. Default is `TRUE`.
check_fields <- function(data, all_fields = TRUE) {
  if (all_fields) {
    check_required_fields(data)
    check_recommended_fields(data)
  } else {
    check_required_fields(data)
  }
  check_percent_match(data)
  return(invisible(NULL))
}

#'
#' @rdname check-dwc
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
      x = glue::glue("\nMissing column(s):\n - {list_of_unmatched}")
    )
    message(bullets)
  } else {
    message("All required fields present...")
  }
  return(invisible(NULL))
}

#' @rdname check-dwc
#' @importFrom rlang inform
check_recommended_fields <- function(data) {
  message("\nChecking fields recommended by Darwin Core...")
  message("---------------------------------------------\n")
  column_names <- colnames(data)
  suggested_cols <- c(
    "kingdom", "taxonRank",
    "decimalLatitude", "decimalLongitude", "geodeticDatum",
    "countryCode",
    "individualCount", "organismQuantity", "organismQuantityType"
  )
  if (!all(column_names %in% suggested_cols)) {
    unmatched <- names(data[, !column_names %in% suggested_cols])
    list_of_unmatched <- glue::glue_collapse(unmatched,
      sep = "\n - "
    )
    bullets <- c(
      "Missing suggested columns.\n",
      i = "Darwin Core standards recommend that the following columns are supplied:\n",
      x = glue::glue("\nMissing column(s):\n - {list_of_unmatched}")
    )
    message(bullets)
  } else {
    message("All recommended fields present...")
  }
  return(invisible(NULL))
}

# TODO: the check will perform at the end, using the data that has by now been
# potentially renamed / updated etc. However, a user may also like to use stand
# alone before running the complete check, to get a quick overview.
#' @rdname check-dwc
#' @importFrom crayon green red
#' @importFrom glue glue
#' @importFrom rlang abort
#' @importFrom rlang inform
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
