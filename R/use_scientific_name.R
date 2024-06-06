#' Add scientific name and authorship to a `tibble`
#' 
#' Format the field `scientificName`, the lowest identified taxonomic name of an 
#' occurrence, along with the rank and authorship of the provided name.
#' @param df a `data.frame` or `tibble` that the column should be appended to.
#' @param scientificName The full scientific name in the lower level taxonomic 
#' rank that can be determined.
#' @param scientificNameRank The taxonomic rank of `scientificName`.
#' @param scientificNameAuthorship The authorship information for `scientificName`.
#' @param .keep Control which columns from .data are retained in the output. 
#' Note that unlike `dplyr::mutate`, which defaults to `"all"` this defaults to 
#' `"unused"`; i.e. only keeps Darwin Core fields, and not those fields used to 
#' generate them.
#' @returns A tibble with the requested fields added.
#' @details
#' Examples of `scientificName` values (we specify the rank in parentheses, but 
#' users should not include this information):
#' * `Coleoptera` (order)
#' * `Vespertilionidae` (family)
#' * `Manis` (genus)
#' * `Ctenomys sociabilis` (genus + specificEpithet)
#' * `Ambystoma tigrinum diaboli` (genus + specificEpithet + infraspecificEpithet)
#' 
#' Examples of `scientificNameRank`:
#' * `order`
#' * `genus`
#' * `subspecies`
#' * `infraspecies`
#' 
#' Examples of `scientificNameAuthorship`:
#' * `(Györfi, 1952)`
#' * `R. A. Graham`
#' * `(Martinovský) Tzvelev`
#' 
#' @seealso [use_taxonomy()] for taxonomic name information.
#' @importFrom dplyr mutate
#' @importFrom rlang abort
#' @export
use_scientific_name <- function(
    df,
    scientificName = NULL,
    scientificNameRank = NULL,
    scientificNameAuthorship = NULL,
    .keep = "unused"
){
  if(missing(df)){
    abort("df is missing, with no default")
  }
  result <- df |>
    mutate(scientificName = {{scientificName}},
           scientificNameRank = {{scientificNameRank}},
           scientificNameAuthorship = {{scientificNameAuthorship}},
           .keep = .keep)
  check_scientificname(df, level = "abort")
  # check_scientificNameRank(df, level = "abort")
  # check_scientificNameAuthorship(df, level = "abort")

  result
}