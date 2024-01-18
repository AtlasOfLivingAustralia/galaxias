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
#' **Setting up an archive**
#'
#'   * [dwca()] create pipable `dwca` objects
#'   * [add_occurrences()], [add_events()], [add_multimedia()], [add_metadata()] to add data to `dwca` objects
#'   
#' **Data handling**
#'  
#'   * [build_random_identifier()], [build_composite_identifier()] uuids
#'   * [get_blank_metadata()], [get_example_metadata()] get example metadata in xml
#'   * [read_md()], [write_md()] to convert between markdown and xml
#'    
#'  **Checking and publication**
#'    * [build()] build a `dwca` into a zip file
#'    * [check()] check a `dwca` object locally or via API
#'    * [publish()] send your data to the ALA for publication
#' 
"_PACKAGE"