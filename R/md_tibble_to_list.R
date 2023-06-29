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

md_tibble_to_list <- function(metadata_tibble){
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