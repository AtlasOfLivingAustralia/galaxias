#' Build repositories to share biodiversity data
#' 
#' @description
#' `galaxias` helps users describe, package and share biodiversity 
#' information using the 'Darwin Core' data standard, which is the format used
#' and accepted by the Global Biodiversity Information Facility (GBIF) and its'
#' partner nodes. `galaxias` is functionally similar to `devtools`, but with a focus on
#' building Darwin Core Archives rather than R packages.
#' 
#' The package is named for a genus of freshwater fish.
#' 
#' @name galaxias-package
#' @docType package
#' @references If you have any questions, comments or suggestions, please email
#' [support@ala.org.au](mailto:support@ala.org.au).
#' 
#' **Prepare information for Darwin Core**
#'   * [use_metadata_template()] Add a blank metadata statement template to the working directory
#'   * [suggest_workflow()] Advice to standardise data using the Darwin Core Standard
#'
#' **Add information to a folder**
#'   * [use_data()] Save standardised data for use in a Darwin Core Archive
#'   * [use_metadata()] Convert a metadata file from markdown to EML (`eml.xml`) and save for use in a Darwin Core Archive
#'   * [use_schema()] Build a schema file (`meta.xml`) for a given directory and save for use in a Darwin Core Archive
#'   
#' **Build an archive**
#'   * [build_archive()] Convert a directory to a Darwin Core Archive
#'   
#' **Verify an archive meets Darwin Core Standard**
#'   * [check_directory()] Check files in your local Darwin Core directory
#'   * [check_archive()] Check whether archive passes Darwin Core criteria via the GBIF API
#'   * [galaxias_config()] Store credentials for your API call
#'   * [print_report()] Methods for displaying API responses
#'   
#' @keywords internal
"_PACKAGE"