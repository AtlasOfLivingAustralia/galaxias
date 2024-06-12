#' @noRd
#' @keywords Internal
parse_xml_to_md <- function(x){
  parse_xml_to_tibble(x) |>
    parse_tibble_to_md()
}

#' @importFrom xml2 as_list
#' @noRd
#' @keywords Internal
parse_xml_to_tibble <- function(x){
  as_list(x) |>
    parse_list_to_tibble()
}

#' @importFrom xml2 as_list
#' @noRd
#' @keywords Internal
parse_xml_to_list <- function(x){
  as_list(x)
}