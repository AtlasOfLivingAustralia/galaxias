#' Read markdown format to a `tibble`
#' 
#' Markdown documents can be parsed to xml if we assume that the header level
#' (typically shown using '#') reflects the nestedness structure of the 
#' dataset. `read_md()` converts markdown to a tibble. If html tags are used in 
#' the document (e.g. `<h1>` etc.), their attributes are retained in the 
#' resulting outputs.
#'
#' Code blocks are not formally supported, so please use text only!
#' @param file A markdown file
#' @returns An object of class `tbl_df`, `tbl` and `data.frame`, aka a `tibble`,
#' containing the following fields:
#' 
#'  * `level` header level
#'  * `label` header label
#'  * `attributes` any html node attributes
#'  * `text` any text below that heading, noting empty headers are not retained.
#' @export
read_md <- function(file){
  readLines(file) |>
    parse_md_to_tibble() # slightly quicker than calling `parse_as_tibble()`
}
# TODO:
# "dateStamp" gets parsed to "Date Stamp" (xml > rmd) but not back again, and is lost
# attributes within source xml are not retained to rmd
# need to add attributes support for markdown headings
# issue where empty titles are not preserved in parse_list_to_tibble()