#' wrapper function to convert xml to a .rmd file
#' @param xml An `xml` object imported using `xml2`
#' @param file (string) file name to save text, typically to `.md` or `.Rmd`
#' @importFrom xml2 as_list
#' @importFrom glue glue
#' @export
xml_to_rmd <- function(xml, file){
  # add a boilerplate `rmd` header to support rendering
  # note: possible instead to populate header with dataset title, date
  header <- glue("---
title: \"Darwin Core Metadata\"
output: 
  html_document:
    toc: true
    toc_float: true
date: \"{Sys.Date()}\"
---")
  cat(header, file = file)
  input <- xml2::as_list(x)$eml
  md_recurse(input, file = file)
}

#' Internal recursive function
#' @param x (list) A list constructed from xml (via `xml2::as_list()`)
#' @param level (integer) what depth are we currently in within the list
#' @param file (string) file name to save changes
#' @importFrom rlang abort
#' @importFrom snakecase to_title_case
#' @noRd
#' @keywords Internal
md_recurse <- function(x, level = 1, file){
  if(!inherits(x, "list")){
    abort("")
  }
  if(missing(file)){
    abort("`file` is missing, with no default")
  }
  x_names <- names(x)
  if(is.null(x_names)){
    x |>
      unlist() |>
      paste(collapse = "\n") |>
      paste0("\n") |>
      cat(file = file, append = TRUE)
  }else{
    invisible(lapply(seq_along(x), function(a){
      prefix <- paste(rep("#", level), collapse = "")
      if(x_names[a] != ""){
        paste0("\n", 
               prefix, 
               " ",
               to_title_case(x_names[a]), 
               "\n") |>
          cat(file = file, append = TRUE)        
      }
      if(is.list(x[[a]])){
        md_recurse(x[[a]], level = level + 1, file = file)  
      }else if(all(!grepl("^(\\s|\\n|\\t)*$", x[[a]]))){
        x[[a]] |>
          paste0("\n") |>
          cat(file = file, append = TRUE)        
      }
    }))   
  }
}

# # test with real XML
# library(xml2)
# x <- read_xml("https://collections.ala.org.au/ws/eml/dr368")
# xml_to_md(x, "test.md")
# md_to_xml("test.md") |> str()

# NOTES:
  # attributes are not preserved. This is potentially a problem when e.g. urls are stored as attributes
  # boilerplate content for eml headers is also not given yet