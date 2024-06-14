#' @noRd
#' @keywords Internal
parse_xml_to_md <- function(x){
  x |>
    parse_xml_to_list() |>
    parse_list_to_tibble() |>
    parse_tibble_to_md()
}

#' @importFrom xml2 as_list
#' @noRd
#' @keywords Internal
parse_xml_to_tibble <- function(x){
  x |>
    parse_xml_to_list() |>
    parse_list_to_tibble()
}

#' @importFrom xml2 as_list
#' @noRd
#' @keywords Internal
parse_xml_to_list <- function(x){
  x |>
    as_list() |>
    xml_to_list_recurse() 
}

#' if we only use xml2::as_list, text is stored in a list underneath it's heading,
#' rather than the heading being the name for that attribute. Hence we need a
#' function to 'collapse' the last entry in a list an place it with its' heading.
#' @importFrom purrr map
#' @noRd
#' @keywords Internal
xml_to_list_recurse <- function(x){
  map(.x = x,
      .f = \(a){
        if(is.list(a)){
          if(length(a) == 1){
            if(inherits(a[[1]], "character")){
              a[[1]]
            }else{
              xml_to_list_recurse(a)
            }
          }else{
            xml_to_list_recurse(a)
          }
        }else{
          a[[1]]
        }
      })
}
# TODO: doesn't retain attributes on nodes