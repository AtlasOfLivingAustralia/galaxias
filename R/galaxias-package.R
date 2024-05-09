#' Build repositories to share biodiversity data
#' 
#' @description
#' `galaxias` provides tools to build repositories optimized to store, document, 
#' and share biodiversity information. 
#'  
#' Once the repository is built and populated with biodiversity data, `galaxias` 
#' also provides tools to assess compatibility of the data with the 
#' 'Darwin Core' standard, convert the package into a valid 
#' 'Darwin Core Archive', and publish that archive to a data infrastructure 
#' (though currently only the Atlas of Living Australia is formally supported).
#' 
#' `galaxias` is named for a genus of freshwater fish.
#' 
#' @name galaxias-package
#' @docType package
#' @references If you have any questions, comments or suggestions, please email
#' [support@ala.org.au](mailto:support@ala.org.au).
#'
#' @section Functions:
#' **Building your project**
#' 
#' There are two functions that you can use to create your workspace:
#' 
#'   * [create_bd_project()] to create an RStudio project (recommended)
#'   * [create_bd_package()] to create an R package (advanced)
#' 
#' Alternatively, you can call the following functions to add specific content
#' to an existing directory:
#'   
#'   * [use_bd_citation()] to give advice on citing your repository
#'   * [use_bd_data()] to create the requisite 'processed' data files
#'   * [use_bd_data_raw()] build `data-raw` folder, with example code
#'   * [use_bd_description()] add a `DESCIRPTION` file
#'   * [use_bd_metadata()] to create a metadata statement in the home directory
#'   * [use_bd_readme_rmd()] Add a `README.Rmd` file with advice for biodiversity data
#'   * [use_bd_schema()] to create a schema based on objects in the `data` folder
#'   * [use_bd_testthat()] add tests for conformance with Darwin Core standards
#'   * [use_bd_vignette()] add a vignette that summarizes data in your repository
#'   
#' Note the syntax here is based on` usethis`.
#'   
#' **Data manipulation functions**
#'  
#'  These functions assist with populating specific Darwin Core fields.
#'  Q: should these have `use_` prefix? `add_`?
#'  NOTE: should integrate properly with `mutate()` and friends.
#'  
#'   * [use_occurrenceID()] add a unique identifier, either using random numbers or a composite of existing columns
#'   * [use_basisOfRecord()] add a basisOfRecord field
#'   * [use_countryCode()]
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
#'   * [build_dwca()] convert a Biodiversity Data package to a Darwin Core Archive (DwCA)
#'   * [validate_dwca()] check your archive via the ALA validate API (NOT FUNCTIONAL)
#'   * [publish_dwca()] send your data to the ALA for publication (NOT FUNCTIONAL)
#'   
#' **Internal Functions**
#'   * [read_md()] read a markdown file into R as xml
#'   * [write_md()] convert an xml document in the workspace to a markdown file 
#'  
#' @keywords internal
"_PACKAGE"