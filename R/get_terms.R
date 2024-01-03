#' get DwC terms and format them
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom tawnydragon tdwg
#' @importFrom tawnydragon tdwg_standards
#' @importFrom tawnydragon tdwg_terms
#' @importFrom tawnydragon summarize
#' @noRd
#' @keywords Internal
get_terms <- function(){
  terms <- tdwg() |>
    tdwg_standards(label == "Darwin Core",
                   status == "recommended") |>
    tdwg_terms() |>
    summarize()
  
  parents <- terms |>
    filter(is.na(parent_class) & 
             code %in% parent_class) |>
    select(-parent_class, -status)
  
  terms <- terms |>
    filter(!is.na(parent_class))  
  
  list(parents = parents, terms = terms)
}