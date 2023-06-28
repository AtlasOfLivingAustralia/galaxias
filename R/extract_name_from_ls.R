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
#' @example
#' convert_md_tibble_to_list(metadata_tibble) |> extract_name_from_ls(which = "last")

extract_name_from_ls <- function(list, var = "Name", which){
  if(which == "first"){
    name <- list |> 
      pluck(var) |> 
      word(1)
  }
  
  if(which == "last"){
    name <- list |> 
      pluck(var) |> 
      word(2)
  }
  
  return(name)
  
  # TODO: Option to take name from Creator if Name is not supplied, option to take first full name if multiple names are supplied
}