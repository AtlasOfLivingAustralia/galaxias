#' Building Darwin Core Archives for the Atlas of Living Australia
#' 
#' @description
#' The Darwin Core Archive (DwCA) is a object for storing and transferring
#' biodiversity data and associated metadata, which allows facilities 
#' like the Atlas of Living Australia (ALA) to efficiently integrate and share 
#' openly with the community. Creating DwCAs by hand is a notoriously fiddly
#' task. 'galaxias' enables users to efficiently reformat their data to Darwin
#' Core Standards and generate production-ready DwCA in a reproducible and
#' transparent workflow.
#' 
#' Can we hijack the package concept to build DwCAs? How would that be different?
#' 
#' `galaxias` is named for a genus of Australian freshwater fish.
#' 
#' @name galaxias-package
#' @docType package
#' @references If you have any questions, comments or suggestions, please email
#' [support@ala.org.au](mailto:support@ala.org.au).
#'
#' @section Object classes:
#' `galaxias` supports the following object classes:
#' 
#'   * `dwc_a` Darwin Core Archive objects
#'   * `dwc_df` data frames to be checked against the Darwin Core standard
#'   * `dwc_xml` xml objects to be checked against the Darwin Core standard
#'   
#' All can be created using the `galaxias()` function
#' 
#' @keywords internal
#' @section Functions:
#' **Setting up an archive**
#'
#'   * [dwca()] create pipable `dwca` objects
#'   * [add_occurrences()], [add_events()], [add_multimedia()], [add_metadata()] to add data to `dwca` objects
#'   * [read_dwca()], [write_dwca()] 
#'   
#' **Data handling**
#'  
#'   * [build_random_identifier()], [build_composite_identifier()] uuids
#'   * [get_metadata_template()], [get_metadata_example()] get example metadata in xml
#'   * [read_md()], [write_md()] to convert between markdown and xml
#'    
#'  **Checking and publication**
#'    * [build()] build a `dwca` into a zip file
#'    * [test()] run local tests on an archive
#'    * [check()] check a `dwca` conforms to archive-level standards
#'    * [publish()] send your data to the ALA for publication
#' 
"_PACKAGE"