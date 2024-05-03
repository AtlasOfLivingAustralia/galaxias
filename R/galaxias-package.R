#' Tools for building and sharing Biodiversity Data Packages
#' 
#' @description
#' `galaxias` provides tools to build Biodiversity Data Packages, which are R 
#' packages designed to store, document, and share biodiversity information. 
#' Biodiversity Data Packages are fully compatible with other elements of the R 
#' package workflow, including `devtools` for handling your package-buidling
#' workflow, `usethis` for adding package elements, `pkgdown` for displaying 
#' package contents as webpages.
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
#' @section Functions:
#' **Directory & file type manipulation**
#'
#'    * [create_bd_package()] to create a new Biodiversity Data Package
#'    * [build_dwca()] convert a Biodiversity Data package to a Darwin Core Archive (DwCA) - NOT IMPLEMENTED YET
#'    * [build_bd_package()] convert a DwCA to a Biodiversity Data Package
#'    * [read_md()] read a markdown file into R as xml
#'    * [write_md()] convert an xml document in the workspace to a markdown file
#'    
#' **Add content to Biodiversity Data Packages**
#' 
#' These functions are structurally similar to `usethis` functions, but with an
#' added `_bd_` infix (where `bd` stands for `biodiversity data`).
#'
#' The following functions are called internally by `create_bd_package()`, in 
#' this order:
#'   
#'   * [use_bd_description()] update the description to standard for this package type
#'   * [use_bd_readme()] replace the standard README.Rmd with a type-specific version
#'   * [use_bd_data_raw()] build `data-raw` folder, with example code
#'   
#' The following functions are optional:
#' 
#'   * [use_bd_citation()] to give advice on citing your package
#'   * [use_bd_testthat()] add tests for conformance with Darwin Core standards
#'   
#' **Add Darwin Core fields to an existing data.frame**
#'  
#'  These functions assist with populating specific Darwin Core fields.
#'  Q: should these have `use_` prefix? `add_`?
#'  NOTE: should integrate properly with `mutate()` and friends.
#'  
#'   * [use_occurrenceID()] add a unique identifier, either using random numbers or a composite of existing columns
#'   * [use_basisOfRecord()] add a basisOfRecord field
#'   * use_eventDate
#'   * use_decimalLatitude()
#'   * use_decimalLongtiude()
#'   * etc.
#'   * [dwc_fields()] NOT IMPLEMENTED YET, but would be useful e.g. `df |> select(any_of(dwc_fields()))`
#'  
#'  **testthat integration**
#'  
#'  Again, syntax and utility not locked down yet. Example code in `testthat.R`
#'  
#'   * [test_metadata()] check xml metadata is valid using GBIF (or just call in `vignettes`?)
#'   * [skip_dwc()] report only on DwC objects and fields that are actually present
#'   
#'  **Checking and publication**
#'  
#'  NOTE: these APIs do not function (yet). JWT access required.
#'  
#'   * [validate_dwca()] check your archive via the ALA validate API
#'   * [publish_dwca()] send your data to the ALA for publication
#'   
#' @keywords internal
"_PACKAGE"