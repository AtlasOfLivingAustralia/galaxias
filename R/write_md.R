#' Write a `tibble` to a markdown document
#' 
#' Export format is chosen based on the file suffix; GitHub 
#' (.md), Rmarkdown (.Rmd) and Quarto (.qmd) are supported.
#' @param df A tibble in same format as `read_md()`
#' @param file either a character string naming a file, or a connection open for 
#' writing.
#' @export
write_md <- function(df, file){
  c(
    switch_headers(df),
    parse_tibble_to_md(df)) |>
  writeLines(con = file)

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