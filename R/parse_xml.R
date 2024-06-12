#' @noRd
#' @keywords Internal
parse_xml_to_md <- function(x){
}
# incomplete

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