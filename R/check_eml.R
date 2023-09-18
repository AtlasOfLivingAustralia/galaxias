#' Check for required ALA fields in an EML list
#' @param eml_list
#' @return NULL - console output

check_eml <- function(eml_list) {
  message("Checking required fields...\n")
  present_count <- 0
  missing_fields <- c()
  silently <- lapply(required_fields, function(x) {
    if (length(eml_list$dataset[[x]]) == 0) {
      missing_fields <<- c(missing_fields, x)
      message(paste0(x, " is missing\n"))
    } else {
      message(paste0(x, " is present\n"))
      present_count <<- present_count + 1
    }
  })
  # Calculate the percentage of fields that are present
  percentage_present <- (present_count / length(required_fields)) * 100
  # Display the percentage
  if (percentage_present < 100) {
    formatted_missing_fields <- paste0("- ", missing_fields, collapse = "\n")
    cat(paste0(
      "\033[31mWarning:\033[0m ",
      percentage_present,
      "% of required fields present.\nMissing fields:\n",
      formatted_missing_fields, "\n"
    ))
  } else {
    message("\033[32mSuccess:\033[0m 100% of fields present.")
  }
}

# Vector of field terms required by ALA
required_fields <- c(
  "title",
  "abstract",
  # not sure if referencePublication is equivalent to "citation" in ALA terms
  "referencePublication",
  "creator",
  "intellectualRights"
)
