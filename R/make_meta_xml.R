#' Create core subfield
#'
#' @param x output of Darwinised occurrence data, output from `detect_dwc_columns()`
#' @param file_name file name of DwC standard occurrence data
#'
#' @return xml document 
#' @export
#'
#' @examples
#'   DwC_occurrence_data <- 
#'   tibble(basisOfRecord =  "HUMAN_OBSERVATION",
#'   scientificName = "Galaxias maculatus",
#'   eventDate = "2002-12-07",
#'   decimalLatitude = -33.366551,
#'   decimalLongtitude = 151.47635)
#'   
#'   make_core_xml(DwC_occurrence_data)
make_core_xml <- function(
    x,
    file_name = "occurrence.csv"
){
  
  field_names =  colnames(x)
  
  # Create files sublist
  files <-  list(
    location = file_name
  )
  
  # Create id sublist
  id <- list()
  
  # Set id name and attribute
  attributes(id) <- list(index = "0")
  
  # Create field sublists
  n_fields <- length(field_names)
  field <- lapply(seq_len(n_fields), function(a){
    out <- list()
    attributes(out) <- list(
      index = as.character(a),
      url = paste0("http://rs.tdwg.org/dwc/terms/", field_names[[a]]))
    return(out)
  })
  names(field) <- rep("field", n_fields)
  
  # Join all in same level
  core <- c(files = list(files), id = list(id), field)
  names_core <- names(core)
  
  # Set attributes of core
  attributes(core) <- list(encoding="UTF-8",
                           rowType="http://rs.tdwg.org/dwc/terms/Occurrence",
                           fieldsTerminatedBy=",",
                           linesTerminatedBy="\r\n",
                           fieldsEnclosedBy="&quot;",
                           ignoreHeaderLines="1")
  
  return(set_names(core, names_core))
  
  # TODO: Accept output tibble from `detect_dwc_columns()`
  # filename = occurrence.csv may be hardcoded, not 100% sure need to check with Data team, if so remove as arg
}


