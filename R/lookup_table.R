# TODO: need to prevent duplicate column naming
#' Map User Columns to Darwin Core Terms
#'
#' @description
#' For now, mappings are used to rename columns. Can be modified or extended to
#' output a dictionary for the meta.xml and preserve column names etc.
#'
#' This interactive function assists in mapping user data columns to the
#' standard Darwin Core (DwC) terms. It suggests potential DwC matches for each
#' column name in the user's dataset. Suggestions are generated based on word
#' component matches and string distance metrics to provide the best possible
#' matches.
#'
#' Suggestions are presented to the user in a numbered list, and the user is
#' then prompted to select the correct DwC term. If no correct match was found,
#' the user is prompted to manually input the correct term if known, or skip the
#' column, before continuing to the next column.
#'
#' Note that a minimum of five matches is always returned, even if they are
#' only low confidence matches.
#'
#' @param data data frame Containing a dataset of which to map DwC
#'   terms to.
#'
#' @return data frame With renamed columns based on the mapping. Exiting the
#' mapping early (by using the `exit` command when prompted) will return a data
#' frame with any mappings that were made prior to exiting.
#' @export
map_fields <- function(data) {
  simple_h1("Mapping column names to DwC terms")

  # Initialize an empty named vector to store the mappings
  column_mappings <- setNames(character(ncol(data)), colnames(data))

  # Skip columns w/ valid terms
  skip <- colnames(data)[(colnames(data) %in% dwc_terms_archived$column_name)]

  # For each column in the user data...
  # for (col in colnames(data)) {
  for (index in cli::cli_progress_along(colnames(data))) {
    col <- colnames(data)[index]
    if (col %in% skip) {
      next
    }
    suggestions <- ""
    suggestions_overlap <- ""
    suggestions_distance <- ""
    simple_h2("Mapping column:", index, "-", col)
    # Method 1: word component matches
    overlap_scores <- sapply(dwc_terms_archived$column_name, function(term) {
      word_overlap_score(col, term)
    })
    # Get top suggestions based on word overlap
    if (max(overlap_scores) > 0) {
      filtered <- which(overlap_scores > 0)
      top_indices <- filtered[order(-overlap_scores[filtered])]
      top_indices_limited <- head(top_indices, 5)
      suggestions_overlap <- dwc_terms_archived$column_name[top_indices_limited]
    }

    # Get top suggestions based on string distance
    matches <- stringdist::stringdistmatrix(tolower(col),
      tolower(dwc_terms_archived$column_name),
      method = "jw" # "osa"
    )
    suggestions_distance <- dwc_terms_archived$column_name[order(matches)[1:5]]

    # If suggestions_overlap exists, append to suggestions (unique only)
    if (suggestions_overlap != "") {
      suggestions <- unique(c(suggestions_overlap, suggestions_distance))
    } else {
      suggestions <- suggestions_distance
    }

    # Print suggestions
    # Always prints the top 5 matches (suggestions) based on distance, but will
    # print > 5 matches in cases of unique word overlap method matches
    cli::cli_alert_info("Top suggestions based on your column name:\n")
    cli::cli_ol(suggestions)

    # Ask user for mapping
    instruction <- c("Number (>0)", "0", "Type 'exit'")
    definition <- c(
      "Map a suggested term",
      "No matches, continue to manual input",
      "Exit early (session mappings will be preserved)"
    )
    cat(create_aligned_prompt(instruction, definition))
    input <- readline(prompt = "Please enter your choice: ")

    # Early exit condition
    if (tolower(input) == "exit") {
      cli::cli_alert_info("Exiting early")
      cli::cli_alert_info("Current mappings will be saved.")
      label <- c(
        c(paste("column:", col)),
        c(paste("index:", which(colnames(data) == col)))
      )
      cli::boxx(label)
      break
    }

    # Map column
    selection <- as.integer(input)
    if (!is.na(selection)) {
      if (selection == 0) {
        cat("Enter the correct Darwin Core term or leave blank if unsure: ")
        manual_entry <- readline()
        if (manual_entry != "") column_mappings[col] <- manual_entry
      } else if (selection > 0) {
        column_mappings[col] <- suggestions[selection]
      } else {
        cat("Invalid input. Column skipped.\n")
      }
    } else {
      cat("Invalid input. Column skipped.\n")
    }
  }

  # Filter out empty mappings
  valid_mappings <- column_mappings[column_mappings != ""]
  names(data)[names(data) %in% names(valid_mappings)] <- valid_mappings
  cli::cli_alert_success("Mapping complete")
  cli::cli_alert_success("{length(valid_mappings)} columns mapped")

  if (length(skip) > 0) {
    cli::cli_alert_info("{length(skip)} existing DwC terms skipped:")
    cli_ul("{.var .strong {skip}}")
  }

  return(data)
}

#' Import an existing lookup table for DwC terms
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
#' @keywords internal
import_map <- function(mapping, index, dataset = NULL) {
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

#' Word overlap score
#' @param string1 string
#' @param string2 string
#' @return numeric The number of word components that overlap/match between
#'   words (0 no matches, 1 = one match, etc.)
#' @seealso Called by [map_fields()]; calls [split_into_words()]
#' @keywords internal
word_overlap_score <- function(string1, string2) {
  words1 <- split_into_words(string1)
  words2 <- split_into_words(string2)
  length(intersect(words1, words2))
}

#' Split a string into words
#'
#' Splits on non-alphanumeric characters/spaces and handles camel case by
#' inserting spaces before uppercase letters following lowercase ones. Properly
#' handles acronyms: splits 'languageEnglish', 'languageEN', but not 'EN'.
#' @param string string
#' @return character vector
#' @seealso Called by [word_overlap_score()]
#' @keywords internal
split_into_words <- function(string) {
  string_with_spaces <- gsub("([a-z])([A-Z])", "\\1 \\2", string)
  unlist(strsplit(tolower(string_with_spaces), split = "[^a-zA-Z0-9]+"))
}

# Placeholder - check for matches that are difficult to detect with
# string distance, but common enough that it makes sense to implement specific
# logic. Example: species to scientificName.
unlikely_matches <- function() {

}
