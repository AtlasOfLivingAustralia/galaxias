#' Build repositories to share biodiversity data
#' 
#' @description
#' `galaxias` helps users describe, package and share biodiversity 
#' information using the 'Darwin Core' data standard, which is the format used
#' and accepted by the Global Biodiversity Information Facility (GBIF) and its'
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
#'   * [use_metadata()] Add a blank metadata statement to the working directory
#'   * [suggest_workflow()] Advise on configuring a tibble for the Darwin Core standard
#'
#'  **Construct an archive**
#'   * [build_schema()] Build a schema file (`meta.xml`) for a given directory
#'   * [use_metadata()] Read, convert and save a metadata file from markdown to EML (`eml.xml`)
#'   * [build_archive()] Convert a directory to a Darwin Core Archive
#'   
#'  **Validate an archive**
#'   * [check_archive()] Check your archive using the `delma` and `corella` packages
#'   * [galaxias_config()] Store credentials for your API call
#'   * [validate_archive()] Check your archive using the GBIF 'validator' API
#'   * [print_validation()] Methods for displaying API responses
#'   
#' @keywords internal
"_PACKAGE"