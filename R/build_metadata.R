#' Create a metadata statement for a Darwin Core Archive
#' 
#' A metadata statement lists the owner of the dataset, how it was collected,
#' and how it may used (i.e. it's licence). This function is intended to be 
#' primarily internal, and is called by `build_dwca()`, but is provided for 
#' debugging purposes.
#' @param file path to a metadata statement stored in markdown format (.md).
#' @param project a directory containing Darwin Core data, preferrably built
#' with `use_bd_project()`.
#' @returns Does not return an object to the workspace; called for the side
#' effect of building a file named `meta.xml` in the specified directory.
#' @importFrom xml2 as_xml_document
#' @importFrom xml2 xml_add_child
#' @importFrom xml2 write_xml
#' @export
build_metadata <- function(file, project = ".") {
  
  if(missing(file)){
    abort("`file` is missing, with no default.")
  }
  if(!file.exists(file)){
    abort("`file` doesn't exist in specified location.")
  }
  x <- read_md(file, format = "list")
  
  # convert to xml
  object <- list(`eml:eml` = structure(
    x,
    `xmlns:d` = "eml://ecoinformatics.org/dataset-2.1.0",
    `xmlns:eml` = "eml://ecoinformatics.org/eml-2.1.1",
    `xmlns:xsi` = "http://www.w3.org/2001/XMLSchema-instance",
    `xmlns:dc` = "http://purl.org/dc/terms/",
    `xsi:schemaLocation` = "eml://ecoinformatics.org/eml-2.1.1 http://rs.gbif.org/schema/eml-gbif-profile/1.1/eml-gbif-profile.xsd",
    # system = "ALA-Registry", # needed?
    scope = "system",
    `xml:lang` = "en"
  ))
  result <- as_xml_document(object)
  write_xml(result, 
            glue("{project}/eml.xml"))
}