# Map user columns to Darwin Core terms
# Interactive function
# Suggests DwC terms based on column names
# Suggestions based on two methods: word component matches and string distance
map_to_dwc <- function(data) {
  cli::cli_h1("Mapping terms")
  # Initialize an empty named vector to store the mappings
  column_mappings <- setNames(character(ncol(data)), colnames(data))

  # TODO
  # if any col == DwC, skip, append to a list
  # print these cols were skipped at the end
  skip <- colnames(data)[(colnames(data) %in% dwc_terms_archived$column_name)]
  
  # For each column in the user data
  for (col in colnames(data)) {
    
    if (col %in% skip) {
      next
    }
    cat("Mapping column:", col, "\n")
    cli::cli_h2("Heading 2")

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
    if (exists("suggestions_overlap")) {
      suggestions <- unique(c(suggestions_overlap, suggestions_distance))
    } else {
      suggestions <- suggestions_distance
    }

    # Print suggestions
    # Always prints the top 5 matches (suggestions) based on distance, but will
    # print > 5 matches in cases of unique word overlap method matches
    cat("Top suggestions based on your column name:\n")
    for (i in cli::cli_progress_along(seq_along(suggestions))) {
      cat(i, "-", suggestions[i], "\n")
    }

    # Ask user for mapping
    input <- readline(
      prompt = paste0(
        "> Enter a number corresponding to the right term\n",
        "> Enter 0 for no matching term\n",
        "> Type 'exit' to leave early and save current mappings\n"
      )
    )

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
  # For now, rename columns based on mappings, but could easily be changed to
  # output a dictionary that can be used in the build xml
  # Filter out empty mappings
  valid_mappings <- column_mappings[column_mappings != ""]
  names(data)[names(data) %in% names(valid_mappings)] <- valid_mappings
  cli::cli_alert_success("Mapping complete")
  cli::cli_alert_success("{length(valid_mappings)} columns mapped")

  return(data)
}

# Some matches are difficult using string distance, like Species to
# scientificName, yet this is common naming convention. In these cases, it makes
# sense to have some more targeted checks.
unlikely_matches <- function() {

}
#' Import an existing lookup table, in the format of two named columns, where
#' the first contains the user supplied column names, second containing the
#' matching DwC terms.
import_lookup_table <- function(data) {

}


# Compute word component overlap between two strings
# Returns numeric, the number of word components that overlap (0 no matches, 1 =
# one match, etc.)
word_overlap_score <- function(string1, string2) {
  words1 <- split_into_words(string1)
  words2 <- split_into_words(string2)
  length(intersect(words1, words2))
}

# Split a string into word components
split_into_words <- function(string) {
  # Add spaces to string - camel case handling
  # The regex logic is important and includes some acronym handling
  # languageEnglish is split, languageEN is split, languageENregion is not
  # split, EN is not split.
  string_with_spaces <- gsub("([a-z])([A-Z])", "\\1 \\2", string)
  # Split the string on non-alphanumeric characters and spaces
  unlist(strsplit(tolower(string_with_spaces), split = "[^a-zA-Z0-9]+"))
}
