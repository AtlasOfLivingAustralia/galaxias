#' Detect required fields in metadata template
#'
#' @param metadata_tibble

detect_required_metadata <- function(metadata_tibble) {
  required_md_fields <- c(
    "Title", "Public Short Description",
    "Citation", "Creator", "License"
  )

  # THIS DOES NOT WORK

  required_md_data <- md_tibble |>
    filter(header %in% required_md_fields)

  if (any(required_md_data$joined_str == "")) {
    # TODO: Dax's cool detect columns and report percentage code
  }
}
