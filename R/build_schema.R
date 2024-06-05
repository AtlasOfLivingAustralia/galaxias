#' Create a `schema` for a Darwin Core Archive
#' 
#' A schema is an xml document that maps the files and field names in a DwCA.
#' This function is intended to be primarily internal. It works on csv files
#' in a directory
#' @param project a directory containing Darwin Core data, preferrably built
#' with `use_bd_project()`.
#' @importFrom purrr map
#' @importFrom xml2 xml_add_child
#' @noRd
#' @keywords Internal
build_schema <- function(project = ".") {
  supported_types <- c("occurrences.csv", # Q: put these into a function? Might be more consistent across the package
                       "events.csv",
                       "multimedia.csv")
                       # measurementOrFact could be good too
  present_types <- supported_types |>
    map(\(x) file.exists(x)) |> 
    unlist()
  available_types <- supported_types[present_types]
  
  # create object and add children
  result <- create_archive_xml()
  if(length(available_types) > 1){
    nodes <- map(.x = available_types,
                 .f = \(x){
                   type <- sub(".csv$", "", x)
                   field_names <- get_field_names(x)
                   create_file_index(field_names, 
                                     type, 
                                     core = {x == 1})
                 })
    for(i in seq_along(nodes)){ 
      xml_add_child(result, nodes[[i]])
    }
  }
  result
}

#' simple function to get column names from a csv
#' this could use readr::read_csv, but that seems overkill, so uses scan() instead
#' @importFrom purrr pluck
#' @noRd
#' @keywords Internal
get_field_names <- function(file){
  scan(file, 
       what = "character",
       nlines = 1L,
       quiet = TRUE) |>
    strsplit(split = ",") |>
    pluck(1)
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
    rowType = glue("http://rs.tdwg.org/dwc/terms/{type}"), # REPLACE with class lookup c/o {dwc_terms()}
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
        url = glue("http://rs.tdwg.org/dwc/terms/{field_names[[a]]}") # REPLACE with terms lookup c/o {dwc_terms()}
      )
      return(out)
    })
  names(field) <- rep("field", n_fields)
  field
}
