#' Create meta.xml file
#'
#' A metafile is required when an archive includes any extension files or if a
# single core data file uses non-standard column names in the first (header) row
# of data. See here for description https://dwc.tdwg.org/text/
#' @param data data frame Validated DwC dataset
#' @param file_name file name of DwC standard occurrence data
#' @return xml document
#' @export
#' @examples
#' DwC_occurrence_data <-
#'   tibble(
#'     basisOfRecord = "HUMAN_OBSERVATION",
#'     scientificName = "Galaxias maculatus",
#'     eventDate = "2002-12-07",
#'     decimalLatitude = -33.366551,
#'     decimalLongtitude = 151.47635
#'   )
#'
#' make_core_xml(DwC_occurrence_data)
build_schema <- function(data, file_name = "occurrence.csv") {
  field_names <- colnames(data)
  files <- list(
    location = file_name
  )
  id <- list()

  # Set id name and attribute
  attributes(id) <- list(index = "0")

  # Create field list
  n_fields <- length(field_names)
  field <- lapply(seq_len(n_fields), function(a) {
    out <- list()
    attributes(out) <- list(
      index = as.character(a),
      url = paste0("http://rs.tdwg.org/dwc/terms/", field_names[[a]])
    )
    return(out)
  })
  names(field) <- rep("field", n_fields)

  # Join all in same level
  core <- c(files = list(files), id = list(id), field)
  names_core <- names(core)

  # Set attributes of core
  attributes(core) <- list(
    encoding = "UTF-8",
    rowType = "http://rs.tdwg.org/dwc/terms/Occurrence",
    fieldsTerminatedBy = ",",
    linesTerminatedBy = "\r\n",
    fieldsEnclosedBy = "&quot;",
    ignoreHeaderLines = "1"
  )

  # return(purrr::set_names(core, names_core))
  # export the xml as meta.xml
  browser()
  core_named <- purrr::set_names(core, names_core)
  root_node <- list(root = core_named)
  xml_doc <- xml2::as_xml_document(root_node)
  xml2::write_xml(xml_doc, "meta.xml")
}
