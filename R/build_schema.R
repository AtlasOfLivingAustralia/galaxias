#' Create a `schema` for a `dwca` object
#' 
#' A schema is an xml document that maps the files and field names in a DwCA.
#' This function is intended to be primarily internal; but can be called via
#' `add_schema()` for debugging purposes
#' @param .dwca An object of class `dwca`
#' @importFrom purrr map
#' @importFrom xml2 xml_add_child
#' @noRd
#' @keywords Internal
build_schema <- function(.dwca) {
  supported_types <- c("occurrences", "events", "multimedia") # measurementOrFact could be good too
  available_types <- names(.dwca)
  selected_types <- available_types[available_types %in% supported_types]
  nodes <- map(.x = seq_along(selected_types),
      .f = \(x){
        type <- selected_types[x]
        df <- .dwca[[type]]
        create_file_index(colnames(df), type, core = {x == 1})
      })
  result <- create_archive_xml()
  for(i in seq_along(nodes)){
    xml_add_child(result, nodes[[i]])
  }
  result
}

#' Internal function to build root node for schema
#' @importFrom xml2 as_xml_document
#' @importFrom xml2 xml_set_attrs
#' @noRd
#' @keywords Internal
create_archive_xml <- function(){
  result <- list(archive = list()) |>
    as_xml_document()
  xml_set_attrs(result, 
                c(xmlns ="http://rs.tdwg.org/dwc/text/",
                  metadata="eml.xml"))
  result
}

#' Internal function to map field names of a tibble for schema
#' @importFrom glue glue
#' @importFrom purrr map
#' @importFrom xml2 as_xml_document
#' @importFrom xml2 xml_set_attrs
#' @noRd
#' @keywords Internal
create_file_index <- function(field_names, type, core = TRUE){
  # Join information into a single list
  result <- list(c(create_file_list(type), 
    create_id_list(),
    create_field_list(field_names)))
  names(result) <- ifelse(core == TRUE, "core", "extension")
  # Convert to xml and set attributes 
  result_xml <- as_xml_document(result)
  xml_set_attrs(result_xml, c(
    encoding = "UTF-8",
    rowType = glue("http://rs.tdwg.org/dwc/terms/{type}"), # REPLACE with class lookup c/o {tawnydragon}
    fieldsTerminatedBy = ",",
    linesTerminatedBy = "\r\n",
    fieldsEnclosedBy = "&quot;",
    ignoreHeaderLines = "1"))
  result_xml # return
}

#' Internal function to create file name
#' @importFrom glue glue
#' @noRd
#' @keywords Internal
create_file_list <- function(type){
  list(files = list(
    location = list(
      glue("{type}.csv")
      )
    )
  )
}

#' Internal function to create id field
#' @noRd
#' @keywords Internal
create_id_list <- function(){
  id <- list()
  attributes(id) <- list(index = "0")
  list(id = list(id))
}
# TODO: This doesn't parse correctly any more - unclear why.

#' Internal function to create xml map of column names
#' @noRd
#' @keywords Internal
create_field_list <- function(field_names){
  # Create field list
  n_fields <- length(field_names)
  field <- map(
    .x = seq_len(n_fields), 
    .f = \(a){
      out <- list()
      attributes(out) <- list(
        index = as.character(a),
        url = glue("http://rs.tdwg.org/dwc/terms/{field_names[[a]]}") # REPLACE with terms lookup c/o {tawnydragon}
      )
      return(out)
    })
  names(field) <- rep("field", n_fields)
  field
}
