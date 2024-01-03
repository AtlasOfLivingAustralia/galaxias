#' pointblank-based checking of a `dwca` object
#' 
#' Description to follow.
#' @importFrom pointblank interrogate 
#' @importFrom pointblank col_exists
#' @importFrom pointblank create_agent
#' @importFrom pointblank get_agent_report
#' @importFrom pointblank specially
#' @export
check <- function(.dwca){
  x <- .dwca$data
  
  ## summarizing happens by running `incorporate()` after a `create_informant()` pipe
  # x |>
  #   create_informant() |>
  #   info_columns(everything()) |>
  #   incorporate() |>
  #   get_informant_report()
  ## unlike `get_agent_report()`, here we only have a `gt` export option
  ## so this is useful for generating reports, but not for running tests
  ## in the console.
  
  # testing happens by running `interrogate()` after a `create_agent()` pipe
  x |>
    create_agent() |>
    col_exists(columns = vars("decimalLatitude", 
                              "decimalLongitude",
                              "species",
                              "scientificName")) |>
    # below uses anonymous functions, as these are easier to parse later
    specially(fn = \(a){check_latitude(a)}) |> 
    specially(fn = \(a){check_longitude(a)}) |>
    specially(fn = \(a){check_species_in_atlas(a)}) |>
    # build report and return `tibble`
    interrogate() |>
    get_agent_report(keep = "all")
    #                 display_table = FALSE) # disabled for testing
                                             # final version should output a tibble
}

#' check for decimalLatitude
#' @importFrom pointblank test_col_vals_between
#' @noRd
#' @keywords Internal
check_latitude <- function(a){
  if(any(colnames(a) == "decimalLatitude")){
    test_col_vals_between(object = a,
                          columns = "decimalLatitude",
                          left = -90,
                          right = 90,
                          inclusive = c(TRUE, TRUE))
  }else{
    FALSE
  }
}

#' check for decimalLongitude
#' @importFrom pointblank test_col_vals_between
#' @noRd
#' @keywords Internal
check_longitude <- function(a){
  if(any(colnames(a) == "decimalLongitude")){
    test_col_vals_between(object = a,
                          columns = "decimalLongitude",
                          left = -180,
                          right = 180,
                          inclusive = c(TRUE, TRUE))
  }else{
    FALSE
  }
}

#' check species names
#' @importFrom galah search_taxa
#' @noRd
#' @keywords Internal
check_species_in_atlas <- function(a){
  if(any(colnames(a) == "species")){
    # something with `pour` to change target atlas?
    species_list <- unique(a$species)
    result <- search_taxa(species_list)
    all(!is.na(result$taxon_concept_id))
  }else{
    FALSE
  }
}

# # ## test this:
# .dwca <- list(
#    data = tibble(decimalLatitude = 10,
#                  decimalLongitude = 85))
# result <- .dwca |>
#   galaxias::check()