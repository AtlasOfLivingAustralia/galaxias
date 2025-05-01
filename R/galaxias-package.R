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
#' **Set up a project**
#'   * [galaxias_project()] Set up a project with the necessary folder structure
#'   * [use_metadata_template()] Add a blank metadata statement template to the working directory
#'   * [suggest_workflow()] Advice to standardise data using the Darwin Core Standard
#'
#'  **Construct an archive**
#'  
#'   * [use_data()] Save standardised data for use in a Darwin Core Archive
#'   * [use_metadata()] Convert a metadata file from markdown to EML (`eml.xml`) and save for use in a Darwin Core Archive
#'   * [build_archive()] Convert a directory to a Darwin Core Archive
#'   * [build_schema()] Build a schema file (`meta.xml`) for a given directory and save for use in a Darwin Core Archive
#'   
#'  **Validate an archive**
#'   * [check_archive()] Check your archive using the `delma` and `corella` packages
#'   * [galaxias_config()] Store credentials for your API call
#'   * [validate_archive()] Check your archive using the GBIF 'validator' API
#'   * [print_validation()] Methods for displaying API responses
#'   
#' @keywords internal
"_PACKAGE"