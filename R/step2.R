#' Edit metadata template
#'
#'
#' @return template file opened in RStudio as 
#' @importFrom usethis edit_file
#'
#' @examples
edit_metadata_md <-  function(){
  # This path will eventually be from inst/extdata/user_template.md
  path_to_md_template <- "doc/user_template.md"
  
  edit_file(path_to_md_template)
  
  # TODO: Need a mechanism to save the edited file locally.
  # May need usethis::use_template and usethis::edit_template 
}

#' Load in metadata information into R
#'
#' @param path file path pointing .md metadata template
#'
#' @return tibble containing concatenated strings from each field
#' @export

read_metadata_md <- function(path){
  scan_output <- scan(path,
  what = "character", 
  sep = "\n", 
  blank.lines.skip = FALSE)

return(scan_output)
}

#' Extract metadata information and wrangle into tibble
#'
#' @param scan_output output of `read_metadata_md()`
#'
#' @return tibble where text from each field is concatenated by header
#' @importFrom dplyr tibble mutate slice filter pull select
#' @importFrom stringr str_detect regex trimws
#' @importFrom tidyr nest unnest
#' @importFrom purrr map
#'
#' @examples
#' scan_output <- read_metadata_md("doc/westerband_template.md")
#' extract_md_contents(scan_output)

extract_md_contents <- function(scan_output){
  # Determine which elements belong with which header
  grouped_by_header <- tibble(input = scan_output) |> 
    mutate(group_id = str_detect(input, regex("^#")) |> cumsum())
  
  # A function to concatenate text in each field
  join_text <- function(grouped_by_header){
    grouped_by_header |> 
      slice(2:nrow(grouped_by_header)) |>  # Take anything below the 1st row
      filter(! input == "") |>  # Exclude the empty elements of input 
      pull(input) |>  # Isolate column as vector
      paste(collapse="")  #  Join strings
    
    # TODO: Need to replace double/multiple spaces with single space for nicer output
  }
  
  # Mapping across the group_id
  nested_grouped_by_header <- grouped_by_header |> 
    nest(text = input) |> 
    mutate(joined_str = map(text,
                            ~join_text(.x)))
  
  
  # A function to extract the header from the ## notation
  extract_clean_header <- function(grouped_by_header){
    grouped_by_header |> 
      slice(1) |> 
      pull(input) |> 
      trimws("both") |> 
      str_replace(pattern = "^#{1,}", replacement = "") |>  # Replace any number of # with nothing
      str_replace(pattern = "\\+$|\\*$", replacement = "") |> # Replace + and * with nothing
      trimws("both") 
  }
  
  # Create header variable to denote the section the text is from
  metadata_tibble <- nested_grouped_by_header |> 
    mutate(header = map(text,
                        ~extract_clean_header(.x))) |> 
    select(group_id, header, joined_str,  text) |> 
    unnest(cols = c(header, joined_str)) |> 
    print(n = nrow(nested_grouped_by_header))
  
  return(metadata_tibble)
}

#' Convert metadata tibble into list
#'
#' @param metadata_tibble 
#'
#' @return

convert_md_tibble_to_list <- function(metadata_tibble){
  # Convert tibble to list
  metadata_list <- metadata_tibble |> 
    select(joined_str) |> # Select the text column 
    as.list() |> # Treat as list
    list_transpose() 
  
  # Remove join_str
  cleaned_metadata_list <- map(metadata_list, 
       ~unname(.x))
  
  # Set names to match the template
  set_names(cleaned_metadata_list, metadata_tibble$header)
}


