#' Read markdown format to R
#' 
#' Markdown documents can be parsed to xml if we assume that the header level
#' (typically shown using '#') reflects the nestedness structure of the 
#' dataset. `read_md()` converts markdown to a tibble, list or xml, in that 
#' order. If html tags are used in the document (e.g. `<h1>` etc.), their 
#' attributes are retained in the resulting outputs.
#'
#' Code blocks are not formally supported, so please use text only!
#' 
#' @param file A markdown file
#' @param format (string) What format should the requested `file` be returned
#' in? Defaults to `"tibble"`.
#' @returns A `tibble` by default; optionally an object of class `list` or 
#' `xml_document`.
#' @export
read_md <- function(file, format = c("tibble", "list", "xml")){
  x <- readLines(file)
  format <- match.arg(format)
  switch(format, 
         "tibble" = parse_md_to_tibble(x),
         "list" = parse_md_to_list(x),
         "xml" = parse_md_to_xml(x))
}
# TODO:
# "dateStamp" gets parsed to "Date Stamp" (xml > rmd) but not back again, and is lost
# attributes on `eml` level are lost (internally) before getting to `xml` object
# attributes within source xml are not retained to rmd
# need to add attributes support for markdown headings
# issue where empty titles are not preserved in parse_list_to_tibble()