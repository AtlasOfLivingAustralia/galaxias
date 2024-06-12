#' Parse metadata between common object types
#' 
#' It is important in `galaxias` to be able to move between metadata statements
#' stored as strings, `tibble`s, `list`s, and `xml` documents. Primarily called 
#' by `parse_metadata()`, and ultimately `read_md()` and `write_md()`, these 
#' functions are exported for clarity and debugging purposes.
#'
#' In practice, the sequence of transformations is fixed, being `string` >
#' `tibble` > `list` > `xml` for import from markdown, and the reverse for 
#' export. Nonetheless, it can be useful sometimes to have helper functions that 
#' handle this hierarchy 'under the hood'.
#'
#' Note that parsing between tibbles and lists depends on recursive functions, 
#' and are highly bespoke to the galaxias workflow. We use `xml2::as_list()` to 
#' convert from xml to list, and `xml2::as_xml_document()` for the reverse.
#' @param x The object to be parsed. Should be one of class `character` (md),
#' `data.frame` (tibbles), `list` or `xml_document`.
#' @param to (string) What format should `x` be parsed to? Defaults to `tibble`.
#' @name parse_metadata
#' @order 1
#' @export
parse_metadata <- function(x,
                           to = c("tibble", "list", "xml", "md")){
  switch(match.arg(to),
         "md" = parse_as_md(x),
         "tibble" = parse_as_tibble(x),
         "list" = parse_as_list(x),
         "xml" = parse_as_xml(x))
}

#' @rdname parse_metadata
#' @order 2
#' @export
parse_as_md <- function(x){
  switch(detect_metadata_format(x),
         "md" = x,
         "tibble" = parse_tibble_to_md(x),
         "list" = parse_list_to_md(x),
         "xml" = parse_list_to_md(x))
}

#' @rdname parse_metadata
#' @order 3
#' @export
parse_as_tibble <- function(x){
  switch(detect_metadata_format(x),
         "md" = parse_md_to_tibble(x),
         "tibble" = x,
         "list" = parse_list_to_tibble(x),
         "xml" = parse_list_to_tibble(x))
}

#' @rdname parse_metadata
#' @order 4
#' @export
parse_as_list <- function(x){
  switch(detect_metadata_format(x),
         "md" = parse_md_to_list(x),
         "tibble" = parse_tibble_to_list(x),
         "list" = x,
         "xml" = parse_xml_to_list(x))
}

#' @rdname parse_metadata
#' @order 4
#' @export
parse_as_xml <- function(x){
  switch(detect_metadata_format(x),
         "md" = parse_md_to_xml(x),
         "tibble" = parse_tibble_to_xml(x),
         "list" = parse_list_to_xml(x),
         "xml" = x)
}

#' Microfunction to switch formats
#' @importFrom rlang abort
#' @importFrom rlang caller_env
#' @noRd
#' @keywords Internal
detect_metadata_format <- function(x,
                                   error_call = caller_env()){
  if(inherits(x, "character")){
    "md"
  }else if(inherits(x, "data.frame")){
    "tibble"
  }else if(inherits(x, "list")){
    "list"
  }else if(inherits(x, "xml_document")){
    "xml"
  }else{
    abort("Unknown format requested",
          call = error_call)
  }
}