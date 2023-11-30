#' function to parse md to xml
#' @importFrom snakecase to_lower_camel_case
#' @importFrom tibble tibble
#' @importFrom xml2 as_xml_document
#' @export
rmd_to_xml <- function(file){
  x <- readLines(file)
  is_title <- grepl("^#", x) |> which()
  title_length <- attr(regexpr("^#{1,}", x[is_title]), "match.length")
  titles <- to_lower_camel_case(x[is_title])
  # list_content_id <- grepl("^#", x) |> cumsum()
  content <- x[is_title + 1] # this works because all paragraphs are length-1
  # this won't be generically true, so `list_content_id` + `paste()` is safer
  
  # construct a tibble, then recursively build a list
  result <- tibble(
    id = seq_along(titles),
    depth = title_length,
    name = titles,
    content = content) |>
    xml_recurse()
  
  # convert to xml
  list(eml = result) |>
    as_xml_document()
  # note: still requires attributes etc
}

#' Internal function to recursively build nested lists from a tibble
#' @param x A `tibble` built within `md_to_xml()`
#' @noRd
#' @keywords Internal
xml_recurse <- function(x, level = 1){
  if(nrow(x) == 1){
    x$content
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