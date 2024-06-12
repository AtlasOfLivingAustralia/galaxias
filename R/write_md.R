#' Write from various formats to Markdown
#' 
#' Export format is chosen based on the file suffix; GitHub 
#' (.md), Rmarkdown (.Rmd) and Quarto (.qmd) are supported.
#' @param xml A xml object, either imported using `xml2`, or the url or a valid
#' xml page.
#' @param file (string) file name to save text, typically to `.md` or `.Rmd`
#' @importFrom xml2 as_list
#' @export
write_md <- function(x, file, title = "Title goes here"){
  if(inherits(x, "xml_document")){
    x <- as_list(x)
  }
  # x <- xml2::as_list(xml2::read_xml("https://collections.ala.org.au/ws/eml/dr368"))
  df <- parse_list_to_tibble(x)
  # incomplete past here.
 

}

#' This assumes user can call different object types
#' @noRd
#' @keywords Internal
#' @importFrom stringr str_extract
switch_headers <- function(filename, title){
  file_extension <- basename(filename) |>
    str_extract("\\.[:alpha:]+$")
  switch(file_extension, 
         ".md" = build_md_header(title),
         ".rmd" = build_rmd_header(title),
         ".qmd" = build_qmd_header(title),
         "") # GitHub Markdown doesn't require YAML
}

#' @noRd
#' @keywords Internal
#' @importFrom glue glue
build_md_header <- function(title){
  glue("---
title: {title}
output: github_document
---")
}

#' @noRd
#' @keywords Internal
#' @importFrom glue glue
build_rmd_header <- function(title){
  glue("---
title: {title}
output: 
  html_document:
    toc: true
    toc_float: true
date: \"{Sys.Date()}\"
---")
}

#' @noRd
#' @keywords Internal
#' @importFrom glue glue
build_qmd_header <- function(title){
  glue("---
title: {title}
format:
  html:
    toc: true
---")
}