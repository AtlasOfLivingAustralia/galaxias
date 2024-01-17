#' Convert between markdown and xml
#' 
#' With `write_md()`, export format is chosen based on the file suffix; GitHub 
#' (.md), Rmarkdown (.Rmd) and Quarto (.qmd) are supported. 
#' @importFrom dplyr bind_rows
#' @importFrom snakecase to_lower_camel_case
#' @importFrom tibble tibble
#' @importFrom xml2 as_xml_document
#' @name read_md
#' @export
read_md <- function(file){
  # read file, detect headings
  x <- readLines(file)
  is_title <- grepl("^#|^<h", x) |> which()
  
  # find nestedness level of each heading
  # note this approach allows a mix of `###` and `<hn>`
  title_length_hash <- regexpr("^#{1,}", x[is_title]) |>
    attr("match.length")
  title_length_h <- str_extract(x[is_title], "^<h[:digit:]") |>
    sub("^<h", "", x = _) |>
    as.integer()
  title_length <- pmax.int(title_length_hash, 
                           title_length_h, 
                           na.rm = TRUE)
  
  # parse out attributes 
  equals_check <- grepl("=", x[is_title]) # note: can only happen with `<hn>` structure
  if(any(equals_check)){
    attr_tibble <- lapply(
      strsplit(x[is_title[equals_check]], "="),
      \(x){
        n1 <- sub("^<h[[:digit:]]\\s", "", x[1])
        n2 <- sub(">[[:alpha:]]+</h[[:digit:]]>$", "", x[2]) |>
          gsub("^\"|\"$", "", x = _)
        tibble(attr_title = n1, attr_body = n2)
      }) |>
      bind_rows()
  }else{
    attr_tibble <- NULL
  }
  
  # parse out titles and content
  titles <- x[is_title] |>
    gsub("^#+|^<h[[:digit:]]>|</h[[:digit:]]>$", "", x = _) |>
    to_lower_camel_case()
  # list_content_id <- grepl("^#", x) |> cumsum()
  content <- x[is_title + 1] # this works because all paragraphs are length-1
  # this won't be generically true, so `list_content_id` + `paste()` is safer
  
  # construct a tibble, then recursively build a list
  result <- tibble(
    id = seq_along(titles),
    depth = title_length,
    name = titles,
    content = content)
  
  # convert to xml
  object <- list(`eml:eml` = structure(
    xml_recurse(result), 
    `xmlns:d` = "eml://ecoinformatics.org/dataset-2.1.0",
    `xmlns:eml` = "eml://ecoinformatics.org/eml-2.1.1",
    `xmlns:xsi` = "http://www.w3.org/2001/XMLSchema-instance",
    `xmlns:dc` = "http://purl.org/dc/terms/",
    `xsi:schemaLocation` = "eml://ecoinformatics.org/eml-2.1.1 http://rs.gbif.org/schema/eml-gbif-profile/1.1/eml-gbif-profile.xsd",
    # system = "ALA-Registry", # needed?
    scope = "system",
    `xml:lang` = "en"
  ))
  as_xml_document(object)
}

#' Internal function to recursively build nested lists from a tibble
#' @param x A `tibble` built within `read_md()`
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

# TODO:
  # "dateStamp" gets parsed to "Date Stamp" (xml > rmd) but not back again, and is lost
  # attributes on `eml` level are lost (internally) before getting to `xml` object
  # attributes within source xml are not retained to rmd