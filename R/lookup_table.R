# Map user columns to Darwin Core terms
# Interactive function
# Suggests DwC terms based on column names
# Suggestions based on two methods: word component matches and string distance
map_to_dwc <- function(data) {
  # Initialize an empty named vector to store the mappings
  column_mappings <- setNames(character(ncol(data)), colnames(data))

  # For each column in the user data
  for (col in colnames(data)) {
    cat("Mapping column:", col, "\n")

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

    # Method 2: similarity based on string distance
    matches <- stringdist::stringdistmatrix(tolower(col),
      tolower(dwc_terms_archived$column_name),
      method = "jw" # "osa"
    )
    suggestions_distance <- dwc_terms_archived$column_name[order(matches)[1:5]]
    # If suggestions_overlap exists, append to suggestions
    # Only unique suggestions
    if (exists("suggestions_overlap")) {
      suggestions <- unique(c(suggestions_overlap, suggestions_distance))
    } else {
      suggestions <- suggestions_distance
    }
    # Print suggestions
    cat("Top suggestions based on your column name:\n")
    for (i in 1:length(suggestions)) {
      cat(i, "-", suggestions[i], "\n")
    }

    # Ask user for mapping
    selection <- as.integer(
      readline(
        prompt = paste0(
          "Select a number corresponding to the right term",
          "(or 0 if none of them): "
        )
      )
    )

    # Map column
    if (selection > 0) {
      column_mappings[col] <- suggestions[selection]
    } else {
      cat("Enter the correct Darwin Core term or leave blank if unsure: ")
      manual_entry <- readline()
      if (manual_entry != "") column_mappings[col] <- manual_entry
    }
  }
  # add an early exit clause which will rename and return or not rename and
  # return.
  # Rename columns based on mappings
  renamed_data <- data %>% rename_with(~column_mappings, dplyr::everything())

  return(renamed_data)
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
