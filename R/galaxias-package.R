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
#'  
#'   * [use_dwc()] to give real-time feedback on modifications to your data
#'   * [mutate.dwc_df] dwc-specific version of `dplyr::mutate`
#'   * [rename.dwc_df] dwc-specific version of `dplyr::rename`
#'   * [rename_with.dwc_df] dwc-specific version of `dplyr::rename_with`
#'   * [select.dwc_df] dwc-specific version of `dplyr::select`
#'   
#'  The following micro-functions add single DwC fields. They all call their 
#'  correspondingly named `check_` function internally, but with 
#'  `level = "abort"` set to ensure conformance. This differs from the `dplyr`
#'  functions above, which use `level = "inform"`.
#'  
#'   * [use_occurrenceID()] add a unique identifier, either using random numbers or a composite of existing columns
#'   * [use_basisOfRecord()] add a basisOfRecord field
#'   * [use_countryCode()]
#'   * use_eventDate
#'   * use_decimalLatitude()
#'   * use_decimalLongtiude()
#'   * etc.
#'   
#'  Related:
#'   
#'   * [dwc_fields()] NOT IMPLEMENTED YET, but would be useful e.g. `df |> select(any_of(dwc_fields()))`
#'  
#'  **Checking data for DwC compliance**
#'  
#'  The wrapper function for checking tibbles for Darwin Core complicance is
#'  [check_dwc()]. It calls the following microfunctions:
#'  
#'   * [check_fields] Checks whether non-DwC fields are present
#'   * [check_basisOfRecord] Checks whether `basisOfRecord` contains valid values
#'   
#'  **Checking and publication**
#'  
#'   * [build_dwca()] convert a Biodiversity Data package to a Darwin Core Archive (DwCA)
#'   * [check_dwca()] Function to check whole repository for conformance with DwC (NOT FUNCTIONAL)
#'   * [validate_dwca()] check your archive via the ALA validate API (NOT FUNCTIONAL)
#'   * [publish_dwca()] send your data to the ALA for publication (NOT FUNCTIONAL)
#'   
#' **Internal Functions**
#'   * [read_md()] read a markdown file into R as xml
#'   * [write_md()] convert an xml document in the workspace to a markdown file 
#'  
#' @keywords internal
"_PACKAGE"