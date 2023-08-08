#' Functions to open metadata markdown templates, and read markdown
#' into R for use.
#'
#' @param path file path where metadata template will be copied to
#'
#' @return template file opened in RStudio for user to edit
#' @export
#' @importFrom usethis edit_file
#'
#' @examples
#' \dontrun{
#' edit_metadata_md()
#' }
template_open <- function(path = ".") {
  # Template stored in inst/markdown/user_template.md
  path_to_md_template <- system.file("markdown/user_template.md", package = "galaxias")

  # Copy the template to root of directory
  file.copy(path_to_md_template, ".")

  # Edit in RStudio
  edit_file(paste0(path, "/user_template.md"))

  # TODO:
  # Need a mechanism to rename the copied file, perhaps a menu() prompt and wraps around file.rename()
  # Checks if its existing, if so DO NOT overwrite
  # AS: See my shiny app for ideas
}

#' Read metadata md information into R
#'
#' @param path file path pointing .md metadata template
#'
#' @return tibble containing concatenated strings from each field
#' @keywords internal
#' @examples
#' path_to_md_template <- system.file("markdown/westerband_template.md", package = "galaxias")
#' metadata_ls <- read_metadata_md(path_to_md_template)
#'
template_read <- function(path) {
  scan_output <- scan(path,
    what = "character",
    sep = "\n",
    blank.lines.skip = FALSE
  )

  return(scan_output)
}

# Subfunctions to convert markdown into a tibble

#' Read metadata md information into R
#'
#' @param path file path pointing .md metadata template
#'
#' @return tibble containing concatenated strings from each field
#' @keywords internal
#' @examples
#' path_to_md_template <- system.file("markdown/westerband_template.md", package = "galaxias")
#' metadata_ls <- read_metadata_md(path_to_md_template)
#'
read_metadata_md <- function(path) {
  scan_output <- scan(path,
    what = "character",
    sep = "\n",
    blank.lines.skip = FALSE
  )

  return(scan_output)
}

#' Extract metadata text and wrangle into tibble
#'
#' @param scan_output output of `read_metadata_md()`
#'
#' @return tibble where text from each field is concatenated by header
#' @importFrom dplyr tibble mutate slice filter pull select
#' @importFrom stringr str_detect regex str_replace
#' @importFrom tidyr nest unnest
#' @importFrom purrr map
#' @export
#' @examples
#' scan_output <- read_metadata_md("doc/westerband_template.md")
#' extract_md_contents(scan_output)
template_clean <- function(scan_output) {
  grouped_by_header <- tibble(input = scan_output) |>
    mutate(group_id = str_detect(input, regex("^#")) |> cumsum())

  # A function to concatenate text in each field
  join_text <- function(grouped_by_header) {
    grouped_by_header |>
      slice(2:nrow(grouped_by_header)) |> # Take anything below the 1st row
      filter(!input == "") |> # Exclude the empty elements of input
      pull(input) |> # Isolate column as vector
      paste(collapse = "") #  Join strings

    # TODO: Need to replace double/multiple spaces with single space for nicer
    # output
  }

  # Mapping across the group_id
  nested_grouped_by_header <- grouped_by_header |>
    nest(text = input) |>
    mutate(joined_str = map(
      text,
      ~ join_text(.x)
    ))

  # A function to extract the header from the ## notation
  extract_clean_header <- function(grouped_by_header) {
    grouped_by_header |>
      slice(1) |>
      pull(input) |>
      trimws("both") |>
      str_replace(pattern = "^#{1,}", replacement = "") |> # Replace any number of # with nothing
      str_replace(pattern = "\\+$|\\*$", replacement = "") |> # Replace + and * with nothing
      trimws("both")
  }

  # Create header variable to denote the section the text is from
  metadata_tibble <- nested_grouped_by_header |>
    mutate(header = map(
      text,
      ~ extract_clean_header(.x)
    )) |>
    select(group_id, header, joined_str, text) |>
    unnest(cols = c(header, joined_str)) |>
    print(n = nrow(nested_grouped_by_header))

  return(metadata_tibble)
}

#' Convert metadata tibble into list
#'
#' @param metadata_tibble output from `md_to_tibble()`
#'
#' @return named list with field name as name
#' @importFrom dplyr select
#' @importFrom purrr map list_transpose set_names
#' @export
#' @examples
#' path_to_md_template <- system.file("markdown/westerband_template.md", package = "galaxias")
#' scan_output <- read_metadata_md(path_to_md_template)
#' metadata_tibble <- extract_md_contents(scan_output)
#' metadata_ls <- metadata_tibble |> md_tibble_to_list()
#'
make_list <- function(metadata_tibble) {
  # Convert tibble to list
  metadata_list <- metadata_tibble |>
    select(joined_str) |> # Select the text column
    as.list() |> # Treat as list
    list_transpose()

  # Remove join_str
  cleaned_metadata_list <- map(
    metadata_list,
    ~ unname(.x)
  )

  # Set names to match the template
  set_names(cleaned_metadata_list, metadata_tibble$header)
}

#' Extract first or last name from Name field
#'
#' @param list output from `convert_md_tibble_to_list()`
#' @param var character, name field (Name/Creator)
#' @param which character denoting which part of name to extract (first/last)
#'
#' @return vector of length 1 containing first or last name
#' @keywords internal
#' @importFrom stringr word
#' @importFrom purrr pluck
#' @examples
#' path_to_md_template <- system.file("markdown/westerband_template.md", package = "galaxias")
#' scan_output <- read_metadata_md(path_to_md_template)
#' metadata_tibble <- extract_md_contents(scan_output)
#' convert_md_tibble_to_list(metadata_tibble) |> extract_name_from_ls(which = "last")
#'
extract_name_from_ls <- function(list, var = "Name", which) {
  if (which == "first") {
    name <- list |>
      pluck(var) |>
      word(1)
  }

  if (which == "last") {
    name <- list |>
      pluck(var) |>
      word(2)
  }

  return(name)

  # TODO: Option to take name from Creator if Name is not supplied, option to take first full name if multiple names are supplied
}
