#' @noRd
#' @keywords Internal
parse_tibble_to_md <- function(x){}
# incomplete

#' @noRd
#' @keywords Internal
parse_tibble_to_list <- function(x){
  xml_recurse(x, level = 1)
}

#' @importFrom xml2 as_xml_document
#' @noRd
#' @keywords Internal
parse_tibble_to_xml <- function(x){
  parse_tibble_to_list(x) |>
    as_xml_document()
}

#' Internal function to power `parse_tibble_to_list()`
#' necessary to prevent problems if user sets `level` arg
#' @noRd
#' @keywords Internal
xml_recurse <- function(x, level = 1){
  if(nrow(x) == 1){
    list(x$content)
  }else{
    this_level <- x$depth == level
    x_list <- split(x, cumsum(this_level))
    if(level > 1){
      x_list <- x_list[-1]
    }
    names(x_list) <- x$name[this_level]
    lapply(x_list, function(a){xml_recurse(a, level = level + 1)})    
  }
}