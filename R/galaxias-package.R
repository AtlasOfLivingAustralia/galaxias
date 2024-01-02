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
#' **Prepare data**
#'
#'   * [tibble_to_dwc()]
#' 
#' **Build eml.XML**
#' 
#'   * [edit_metadata_md()] 
#'   * [read_metadata_md()] 
#'   * [md_to_tibble()] 
#'   * [detect_required_metadata_fields()] 
#'   * [md_tibble_to_list()] 
#'   * [update_title()] 
#'   * [update_names()] 
"_PACKAGE"