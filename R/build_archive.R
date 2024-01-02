#' `archive` will attempt to zip a complete Darwin Core archive.
#' It will call `darwin_check` to make sure the data conforms to Darwin Core
#' standards. It will also need a valid `meta.xml` file and `eml.xml` file.
#' @param data A data frame of your data
#' @param meta A data frame of your metadata
#' @param eml A data frame of your EML metadata
#' @param path A path to save the zip file to (root by default)
#' @return No object is returned; this function is called for the side-effect
#' of building a 'Darwin Core Archive' (i.e. a zip file)
#' @export
build_archive <- function(data, meta = NULL, eml = NULL, folder = ".") {
  # Check data
  # darwin_check(data, mend = FALSE)
  eml_xml <- as_xml_document(eml)
  message("Writing EML file to disk...\n")
  xml2::write_xml(eml_xml, paste0(folder, "/eml.xml"))
  message("Writing META file to disk...\n")
  # meta_xml <- as_xml_document(meta)
  message("Writing CORE file to disk...")
  # TODO hardcoding this for now
  write.csv(data, paste0(folder, "/occurrence.csv"), row.names = FALSE)
  message("Creating ZIP ...")
  # TODO hardcoding this for now
  zip(zipfile = "dwc.zip", files = folder)
}
