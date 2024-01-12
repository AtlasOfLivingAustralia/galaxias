#' Building Darwin Core Archives for the Atlas of Living Australia
#' 
#' @description
#' The Darwin Core Archive (DwCA) is a transferable unit of 
#' biodiversity data and associated metadata that allows facilities 
#' like the Atlas of Living Australia (ALA) to efficiently integrate and share 
#' openly with the community. Creating DwCAs by hand is a notoriously fiddly,
#' task. 'galaxias' enables users to efficiently reformat their data to Darwin
#' Core Standards and generate production-ready DwCA in a reproducible and
#' transparent workflow.
#' 
#' `galaxias` is named for a genus of Australian freshwater fish.
#' 
#' @name galaxias
#' @docType package
#' @references If you have any questions, comments or suggestions, please email
#' [support@ala.org.au](mailto:support@ala.org.au).
#'
#' @keywords internal
#' @section Functions:
#' **Set up a DWCA**
#'
#'   * [dwca()] create pipable `dwca` objects
#'   * [add_occurrences()], [add_events()], [add_multimedia()], [add_metadata()] to add data to `dwca` objects
#'   
#' **Construct identifiers**
#'  
#'   * [build_random_identifier()], [build_composite_identifier()] uuids
#'   
#' **Metadata handling**
#' 
#'    * [get_metadata_file()] get example or boilerplate metadata file as md
#'    * [read_md()], [write_md()] to convert between markdown and xml
#'    
#'  **Concluding**
#'    * [check_dwca()] check a `dwca` object for compliance with DwC
#'    * [build_dwca] build a `dwca` into a zip file
#' 
"_PACKAGE"