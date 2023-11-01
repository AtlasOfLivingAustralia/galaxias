col <- colnames(data)[3]
map_to_dwc <- function(data) {
  # Initialize an empty named vector to store the mappings
  column_mappings <- setNames(character(ncol(data)), colnames(data))

  # For each column in the user data
  for (col in colnames(data)) {
    cat("Mapping column:", col, "\n")

    # Find the most similar Darwin Core terms based on string distance
    matches <- stringdist::stringdistmatrix(tolower(col),
      tolower(dwc_terms_archived$column_name),
      method = "jw" # "osa"
    )
    suggestions <- dwc_terms_archived$column_name[order(matches)[1:5]]

    # Print suggestions
    cat("Top 5 suggestions based on your column name:\n")
    for (i in 1:length(suggestions)) {
      cat(i, "-", suggestions[i], "\n")
    }

    # Ask user for mapping
    selection <- as.integer(
      readline(
        prompt = "Select a number corresponding to the right term (or 0 if none of them): "
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

  # Rename columns based on mappings
  renamed_data <- data %>% rename_with(~column_mappings, everything())

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

# Split a string into word components
split_into_words <- function(string) {
  unlist(strsplit(tolower(string), split = "[^a-z]+"))
}

# Compute word component overlap between two strings
word_overlap_score <- function(string1, string2) {
  words1 <- split_into_words(string1)
  words2 <- split_into_words(string2)
  length(intersect(words1, words2))
}
