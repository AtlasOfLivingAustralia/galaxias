#' Build repositories to share biodiversity data
#' 
#' @description
#' `galaxias` helps users describe, package and share biodiversity 
#' information using the 'Darwin Core' data standard, which is the format used
#' and accepted by the Global Biodiversity Information Facility (GBIF) and it's
#' partner nodes. It is functionally similar to `devtools`, but with a focus on
#' building Darwin Core Archives rather than R packages.
#' 
#' The package is named for a genus of freshwater fish.
#' 
#' @name galaxias-package
#' @docType package
#' @references If you have any questions, comments or suggestions, please email
#' [support@ala.org.au](mailto:support@ala.org.au).
#' 
#' **Set up a project**
#'   * [galaxias_project()] Set up a project with the necessary folder structure
#'
#'  **Construct an archive**
#'   * [build_schema()] Build a schema file (`meta.xml`) for a given directory
#'   * [build_metadata()] Supply a metadata file in .md format, and convert to a metadata file (`eml.xml`)
#'   * [build_archive()] Convert a directory to a Darwin Core Archive
#'   
#'  **Validate an archive**
#'   * [check_archive()] Check your archive using the `paperbark` and `corella` packages
#'   * [galaxias_config()] Store credentials for your API call
#'   * [validate_archive()] Check your archive using the GBIF 'validator' API
#'   * [print_validation()] Methods for displaying API responses
#'   
#' @keywords internal
"_PACKAGE"