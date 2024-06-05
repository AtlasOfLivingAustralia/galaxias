#' Build repositories to share biodiversity data
#' 
#' @description
#' `{galaxias}` is a `{usethis}` extension for building 'Darwin Core Archives'; a
#' global data standard to store, document, and share biodiversity information.
#' The package provides tools build data projects, manipulate data to 
#' conform with the standard, and check validity against established norms.
#' 
#' The package is named for a genus of freshwater fish.
#' 
#' @name galaxias-package
#' @docType package
#' @references If you have any questions, comments or suggestions, please email
#' [support@ala.org.au](mailto:support@ala.org.au).
#'
#' @section Functions:
#' **Building your project**
#' 
#' The default method to start your project is to call [create_bd_project()]. 
#' Alternatively, you can call the following functions to add specific content
#' to an existing directory:
#'   
#'   * [use_bd_data()] to create the requisite 'processed' data files
#'   * [use_bd_data_raw()] build `data-raw` folder, with example code
#'   * [use_bd_description()] add a `DESCIRPTION` file
#'   * [use_bd_metadata()] to create a metadata statement in the home directory
#'   * [use_bd_readme_md()], [use_bd_readme_rmd()] Add a `README.Rmd` file with advice for biodiversity data
#'   
#' **Add Darwin Core Terms**   
#'
#'  The following functions add single DwC fields, or collections of related 
#'  fields, to an existing `tibble`.
#'  
#'   * [use_coordinates()] for spatial data
#'   * [use_eventDate()] for temporal data
#'   * [use_locality()] for spatial descriptions
#'   
#'   Proposed:
#'   * [use_occurrences()] basic information on observations (occurrenceID, basisOfrecord, recordID (?))
#'   * [use_events()] basic information on observation events (eventID, parentEventID)   
#'   * [use_scientificName()] record the highest level of taxonomic specificity in the dataset (scientificName, scientificNameRank, scientificNameAuthorship)
#'   * [use_taxonomy()] to specify higher taxonomic columns (kingdom, phylum, class, order, family, genus, species, specificEpithet, vernacularName)
#'   * [use_abundance()] to state how many animals were seen during the observation (individualCount, organismQuantity, organismQuantityType, occurrenceStatus)
#'
#'   Possible functions for added functionality:
#'   * [use_darwin_core()] to subset to only fields with DwC names (i.e. same as `df |> select(any_of(dwc_fields()))`)
#'   * [use_individuals()] attributes of individuals measured (individualID, lifeStage, sex, vitality, reproductiveCondition)
#'   * [use_observer()] to specify who made the observation (recordedByID, recordedBy)
#'   * [use_collection()] to give museum- or collection- specific information (datasetID, datasetName, catalogNumber)
#'   * [use_measurement()] for 'Measurement or Fact' data (optional rn)
#'   * [use_media()] good idea, but unclear how users would supply said media; should be urls, but to where?
#'  
#'  **Checking data for Darwin Core compliance**
#'  
#'  The wrapper function for checking tibbles for Darwin Core compliance is
#'  [check_dwc()]. It calls the following microfunctions:
#'   
#'   * [check_dwc()] to run all applicable `check_` functions
#'   * [check_fields()] Checks whether non-DwC fields are present
#'   * [check_occurrenceID()]
#'   * [check_basisOfRecord()]
#'   * [check_continent()]
#'   * [check_country()]
#'   * [check_countryCode()]
#'   * [check_decimalLatitude()]
#'   * [check_decimalLongitude()]
#'   
#'  Note that there are more `check_` functions than `use_` functions, because
#'  some `use_` functions affect multiple fields.
#'   
#'  **Checking and publication**
#'  
#'   * [build_dwca()] convert a Biodiversity Data package to a Darwin Core Archive (DwCA)
#'   * [check_dwca()] Function to check whole repository for conformance with DwC
#'   * [validate_dwca()] check your archive via the ALA validate API
#'  
#' @keywords internal
"_PACKAGE"