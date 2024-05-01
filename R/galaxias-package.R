#' Tools for building and sharing Biodiversity Data Packages
#' 
#' @description
#' `galaxias` provides tools to build Biodiversity Data Packages, which are R 
#' packages designed to store, document, and share biodiversity information. 
#' Biodiversity Data Packages are fully compatible with other elements of the R 
#' package workflow, including `devtools` for building, `usethis` for adding 
#' package elements, `pkgdown` for converting package contents into webpages.
#' 
#' Once a package is built, `galaxias` also provides tools to assess 
#' compatibility of the data with the 'Darwin Core' standard, convert the 
#' package into a valid 'Darwin Core Archive', and publish that archive to a 
#' data infrastructure (though currently only the Atlas of Living Australia is 
#' formally supported).
#' 
#' `galaxias` is named for a genus of Australian freshwater fish.
#' 
#' @name galaxias-package
#' @docType package
#' @references If you have any questions, comments or suggestions, please email
#' [support@ala.org.au](mailto:support@ala.org.au).
#'
#' @section `usethis` extensions:
#' 
#' These functions can be identified as being structurally similar to `usethis`
#' functions, but with an added `_data_` infix.
#'
#'   * [create_data_package()] to create a new Biodiversity Data Package
#'   * [use_data_citation()] to give advice on citing your package
#'   * [use_data_description()] update the description to standard for this package type
#'   * [use_data_examples()] build `data-raw` and `data` folders, with example code
#'   * [use_data_metadata()] add a metadata statement to `vignettes` folder
#'   * [use_data_readme()] replace the standard README.Rmd with a type-specific version
#'   * [use_data_schema()] not implemented yet
#'   * [use_data_tests()] add tests for conformance with Darwin Core standards
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
#'   * [dwca()] create pipeable `dwca` objects
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