#' @importFrom purrr map
#' @noRd
#' @keywords Internal
parse_tibble_to_md <- function(x){
  rows <- nrow(x)
  y <- split(x, seq_len(rows))
  result <- map(.x = y,
      .f = \(a){
          c(format_md_header(a), 
            format_md_text(a$text))
      }) |>
    unlist()
  names(result) <- NULL
  result
}

#' @noRd
#' @keywords Internal
parse_tibble_to_list <- function(x){
  tibble_to_list_recurse(x, level = 1)
}

#' @importFrom xml2 as_xml_document
#' @noRd
#' @keywords Internal
parse_tibble_to_xml <- function(x){
  x |>
    parse_tibble_to_list() |>
    as_xml_document()
}

#' Internal function called only by `parse_tibble_to_md()`
#' @noRd
#' @keywords Internal 
format_md_header <- function(a){
  if(is.na(a$attributes)){
    hashes <- strrep("#", a$level)
    glue("{hashes} {a$label}")
  }else{
    z <- a$attributes[[1]]
    attributes <- paste(names(z), z, sep = "=") |>
      paste(collapse = " ")
    glue("<h{a$level} {attributes}>{a$label}</h{a$level}>")
  }
}

#' Internal function called only by `parse_tibble_to_md()`
#' @noRd
#' @keywords Internal 
format_md_text <- function(string){
  if(is.na(string)){
    ""
  }else{
    c("", string, "")
  }
}

#' Internal function to power `parse_tibble_to_list()`
#' necessary to prevent problems if user sets `level` arg
#' @importFrom purrr map
#' @noRd
#' @keywords Internal
tibble_to_list_recurse <- function(x, level = 1){
  if(nrow(x) == 1){
    x$text
  }else{
    this_level <- x$level == level
    x_list <- split(x, cumsum(this_level))
    if(level > 1){
      x_list <- x_list[-1]
    }
    names(x_list) <- x$label[this_level]
    map(.x = x_list, 
        .f = \(a){tibble_to_list_recurse(a, level = level + 1)})    
  }
}