#' get DwC terms and format them
#' @importFrom dplyr bind_rows
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom purrr pluck
#' @importFrom tawnydragon tdwg
#' @importFrom tawnydragon tdwg_standards
#' @importFrom tawnydragon tdwg_terms
#' @importFrom tawnydragon summarize
#' @noRd
#' @keywords Internal
get_terms <- function(){
  terms_full <- tdwg() |>
    tdwg_standards(label == "Darwin Core",
                   status == "recommended") |>
    tdwg_terms() |>
    pluck("terms")
  
  parents <- terms_full |>
    filter(is.na(parent_class) & 
             code %in% parent_class) |>
    select("code", "description")
  
  terms <- terms_full |>
    filter(!(code %in% parents$code),
           type == "term") |>
    select("parent_class", "code", "label", "description", "examples")
  terms$parent_class[is.na(terms$parent_class)] <- "No parent class"
  
  parents <- bind_rows(parents, 
                       tibble(code = "No parent class", 
                              description = "None given"))
  
  list(parents = parents, terms = terms)
}