#' Detect if a unique-like column is found in the data
#'
#' Unique columns are detected (every row is unique). If yes, the user is given
#' the option to rename it. If no unique column is detected, the user is given
#' the option to create one. A unique identifier column is required to comply
#' with DwC standard.
#' @param data A tibble
#' @return Based on user choice, 1) input tibble with a renamed column, 2) input
#' tibble with a new column, or 3) NULL
detect_unique_column <- function(data) {
  result <- data %>%
    dplyr::summarise(dplyr::across(
      dplyr::everything(),
      ~ dplyr::n_distinct(.) == nrow(data)
    ))
  if (any(result)) {
    unique_columns <- names(result)[unlist(result)]
    sprintf("Column(s) detected with unique values: %s", unique_columns)
    message("Would you like to rename detected column(s)
    to comply with Darwin Core standards?")
    # Add a yes or no prompt here
    # if yes, rename interactive
    # else, return to check sequence
    rename_interactive(data, unique_columns)
  } else {
    message("No unique columns detected.
    Select Yes to create one using an in-built function? Or select No to skip.")
    cat("Choose an action:\n")
    cat("1. Yes\n")
    cat("2. No\n")
    choice <- as.integer(readline("Enter your choice: "))

    if (choice == 1) {
      message("Select method to create unique column:")
      cat("Choose an action:\n")
      cat("1. Random identifier\n")
      cat("2. Composite identifier\n")
      choice <- as.integer(readline("Enter your choice: "))
      if (choice == 1) {
        build_random_identifier(data)
      } else if (choice == 2) {
        message("Select columns to use to create composite identifier:")
        cat("Available columns:\n")
        for (i in 1:length(colnames(data))) {
          cat(paste(i, ": ", colnames(data)[i], "\n", sep = ""))
        }
        # Get user input
        selected_indices <- readline("Please enter the comma separated indices
        of the columns you want to select (e.g. 1, 3): ")

        # Convert the comma-separated string to a vector of integers
        # TODO: Not robust because it requires a space after the comma
        selected_indices <- as.integer(unlist(strsplit(selected_indices, ", ")))

        # Select the specified columns from the data frame
        selected_cols <- colnames(data)[selected_indices]
        build_composite_identifier(data, cols = selected_cols)
      } else {
        cat("Invalid choice. Exiting...\n")
        return(invisible(NULL))
      }
    } else if (choice == 2) {
      message("No unique column created.
      Please create a unique column to comply with Darwin Core standards.")
    } else {
      cat("Invalid choice. Exiting...\n")
      return(invisible(NULL))
    }
  }
}

#' Rename a column interactively.
#'
#' This is called by `detect_unique_column` when one or more unique columns are
#' detected. The user is then given the option to rename the column to one of
#' the Darwin Core standard identifiers, if it is appropriate to do so.
#' @param data A tibble
#' @param unique_columns A character vector of column names
#' @return A tibble with renamed columns
#' @keywords internal
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
  message("Renamining complete.\n")
  return(invisible(data))
}

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#' Create a random identifier column
#' @param data A tibble
#' @param random_seed Integer: seed value (optional)
#' @keywords internal
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
#' @keywords internal
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
