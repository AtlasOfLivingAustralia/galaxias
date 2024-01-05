#' pointblank-based checking of a `dwca` object
#' 
#' Description to follow.
#' @param .dwca An object of class `dwca`, created using `dwca()`
#' @param max_n Optional limit to the number of failed tests allowed
#' @importFrom pointblank col_exists
#' @importFrom pointblank create_agent
#' @importFrom pointblank get_agent_report
#' @importFrom rlang abort
#' @export
check <- function(.dwca,
                  max_n = NULL){
  if(!inherits(.dwca, "dwca")){
    abort("`check` only accepts `dwca` objects")
  }
  x <- .dwca$data
  x |>
    create_agent(label = "::QUIET::") |> # hack here to turn off reporting
    check_required_cols() |>
    check_recommended_cols() |>
    check_latitude() |>
    check_longitude() |>
    # specially(fn = \(a){check_species_in_atlas(a)}) |>
    # build report and return `tibble`
    galaxias_interrogate(max_n = max_n) |>
    get_agent_report(keep = "all",
                     display_table = FALSE) # disabled for testing
                                             # final version should output a tibble
}

#' Internal function to run `interrogate` with a more limited set of options
#' @importFrom pointblank interrogate
#' @noRd
#' @keywords Internal
galaxias_interrogate <- function(.x, max_n){
  if(is.null(max_n)){
    .x |>
      interrogate(extract_failed = FALSE)
  }else{
    .x |>
      interrogate(extract_failed = TRUE, get_first_n = max_n)
  }
}
# NOTE: max_n currently doesn't limit the number of tests run by `interrogate()`
# unclear why

#' check all required columns are provided
#' @importFrom pointblank specially
#' @noRd
#' @keywords Internal
check_required_cols <- function(.x){
  .x |>
    specially(fn = \(x){
      cols <- c("scientificName", "eventDate", "basisOfRecord")
      all(cols %in% colnames(x))
    })
}

#' check all recommended columns are provided
#' @importFrom pointblank specially
#' @noRd
#' @keywords Internal
check_recommended_cols <- function(.x){
  .x |>
    specially(fn = \(x){
      cols <-  c("kingdom", "taxonRank",
                 "decimalLatitude", "decimalLongitude", "geodeticDatum",
                 "countryCode",
                 "individualCount", "organismQuantity", "organismQuantityType")
      all(cols %in% colnames(x))
    })
}

#' check for decimalLatitude
#' @importFrom pointblank specially
#' @importFrom pointblank test_col_vals_between
#' @noRd
#' @keywords Internal
check_latitude <- function(.x){
  .x |>
    col_exists(columns = "decimalLatitude") |>
    specially(fn = \(a){
      if(any(colnames(a) == "decimalLatitude")){
        test_col_vals_between(object = a,
                              columns = "decimalLatitude",
                              left = -90,
                              right = 90,
                              inclusive = c(TRUE, TRUE))
      }else{
        FALSE
      }   
    })
}

#' check for decimalLongitude
#' @importFrom pointblank specially
#' @importFrom pointblank test_col_vals_between
#' @noRd
#' @keywords Internal
check_longitude <- function(.x){
  .x |>
    col_exists(columns = "decimalLongitude") |>
    specially(fn = \(a){
      if(any(colnames(a) == "decimalLongitude")){
        test_col_vals_between(object = a,
                              columns = "decimalLongitude",
                              left = -180,
                              right = 180,
                              inclusive = c(TRUE, TRUE))
      }else{
        FALSE
      }
    })
}

#' Check for unique identifier columns, and if present, check they are actually unique
#' @importFrom dplyr pull
#' @importFrom pointblank specially
#' @noRd
#' @keywords Internal
check_unique_identifiers <- function(.x){
  .x |>
    specially(fn = \(a){
      cols <- c("occurrenceID", "catalogueNumber", "recordNumber")
      any(cols %in% colnames(a))
    }) |>
    specially(fn = \(a){
      cols <- c("occurrenceID", "catalogueNumber", "recordNumber")
      cols_present <- cols %in% colnames(a)
      if(any(cols_present)){
        lapply(cols[cols_present], \(b){
          x <- a |> pull(b)
          length(x) == length(unique(x))
        }) |>
          unlist() |>
          any()
      }else{
        FALSE
      }
    })
}

#' check species names
#' @importFrom galah search_taxa
#' @noRd
#' @keywords Internal
# check_species_in_atlas <- function(a){
#   if(any(colnames(a) == "species")){
#     # something with `pour` to change target atlas?
#     species_list <- unique(a$species)
#     result <- search_taxa(species_list)
#     all(!is.na(result$taxon_concept_id))
#   }else{
#     FALSE
#   }
# }