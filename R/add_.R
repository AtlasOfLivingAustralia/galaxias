#' Add information to a `dwca` object
#' 
#' These are all pipable functions for adding relevant information to a `dwca`
#' object
#' @param .dwca An object of class `dwca`, constructed with `dwca()`.
#' @param df Optional R object; `tibble` or `data.frame`.
#' @param file Optional file name to be imported with `readr::read_csv()` for
#' `.csv`, `xml2::read_xml()` for `.xml`, or `galaxias::read_md()` for 
#' `.md`, `.rmd` or `.qmd`.
#' @param ... Additional information passed to `read_csv()` or `read_xml()`.
#' @name add_
#' @order 1
#' @importFrom readr read_csv
#' @export
add_data <- function(.dwca, 
                     df,
                     file){
  if(!missing(df)){
    .dwca$data <- df
  }else if(!missing(file)){
    .dwca$data <- read_csv(file, show_col_types = FALSE)
  }
  .dwca
}

#' @name add_
#' @param xml Optional R object imported with `xml2` (class `xml_document`).
#' @order 2
#' @importFrom xml2 read_xml
#' @export
add_metadata <- function(.dwca,
                         xml,
                         file, 
                         ...){
  if(!missing(xml)){
    .dwca$metadata <- xml
  }else if(!missing(file)){
    .dwca$metadata <- read_xml(file, ...)
  }
  .dwca
}

#' @name add_
#' @param xml Optional R object imported with `xml2` (class `xml_document`).
#' @order 3
#' @importFrom xml2 read_xml
#' @export
add_column_mappings <- function(.dwca,
                         xml,
                         file, 
                         ...){
  if(!missing(xml)){
    result <- xml
  }else if(!missing(file)){
    result <- read_xml(file, ...)
  }else{
    result <- build_column_mappings(.dwca)
  }
  .dwca$column_mappings <- result
  .dwca
}