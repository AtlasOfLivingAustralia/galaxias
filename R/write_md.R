
#' @rdname read_md
#' @param xml A xml object, either imported using `xml2`, or the url or a valid
#' xml page.
#' @param file (string) file name to save text, typically to `.md` or `.Rmd`
#' @importFrom xml2 as_list
#' @export
write_md <- function(xml, file, title = "Title goes here"){
  # add a boilerplate `rmd` header to support rendering
  # note: possible instead to populate header with dataset title, date
  cat(switch_headers(file, title), 
      file = file)
  md_recurse(as_list(xml), file = file)
}

#' @noRd
#' @keywords Internal
#' @importFrom stringr str_extract
switch_headers <- function(filename, title){
  file_extension <- basename(filename) |>
    str_extract("\\.[:alpha:]+$")
  switch(file_extension, 
         ".rmd" = build_rmd_header(title),
         ".qmd" = build_qmd_header(title),
         "") # GitHub Markdown doesn't require YAML
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

#' Internal recursive function
#' @param x (list) A list constructed from xml (via `xml2::as_list()`)
#' @param level (integer) what depth are we currently in within the list
#' @param file (string) file name to save changes
#' @importFrom rlang abort
#' @importFrom snakecase to_title_case
#' @noRd
#' @keywords Internal
md_recurse <- function(x, level = 1, file){
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
      if(x_names[a] != ""){
        current_attr <- attributes(x[[a]])
        current_title <- to_title_case(x_names[a])
        if(length(current_attr) > 1){
          header <- glue("<h{level} {names(current_attr)[2]}=\"{current_attr[2]}\">{current_title}</h{level}>")
          paste0("\n", header, "\n") |> # get around bug with `glue` deleting `\n`
            cat(file = file, append = TRUE)  
        }else{
          header <- glue("{strrep('#', level)} {current_title}")
          paste0("\n", header, "\n") |>
            cat(file = file, append = TRUE)  
        }
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
# write_md(pluck(as_list(xml), "eml"), "test.rmd")
# y <- read_md("test.rmd)