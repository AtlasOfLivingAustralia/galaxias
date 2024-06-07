#' Add taxonomic information to a `tibble`
#' 
#' Format fields that contain taxonomic name information from kingdom to 
#' species, as well as the common/vernacular name, to a `tibble`.
#' 
#' In 
#' practice this is no different from using `mutate()`, but gives some 
#' informative errors, and serves as a useful lookup for taxonomic names in 
#' the Darwin Core Standard.
#' @param df a `data.frame` or `tibble` that the column should be appended to.
#' @param kingdom The kingdom name of identified taxon.
#' @param phylum The phylum name of identified taxon.
#' @param class The class name of identified taxon.
#' @param order The order name of identified taxon.
#' @param family The family name of identified taxon.
#' @param genus The genus name of the identified taxon.
#' @param species The species name of the identified taxon.
#' @param specificEpiphet The name of the first species or species epiphet of the `scientificName`.
#' @param vernacularName The common or vernacular name of the identified taxon.
#' @param .keep Control which columns from .data are retained in the output. 
#' Note that unlike `dplyr::mutate`, which defaults to `"all"` this defaults to 
#' `"unused"`; i.e. only keeps Darwin Core fields, and not those fields used to 
#' generate them.
#' @returns A tibble with the requested fields added.
#' @details
#' Examples of `specificEphiphet`:
#' * If `scientificName` is `Abies concolor`, the `specificEpiphet` is `concolor`.
#' * If `scientificName` is `Semisulcospira gottschei`, the `specificEpiphet` is `gottschei`.
#' 
#' @seealso [use_scientific_name()] for adding `scientificName` and authorship information.
#' @importFrom dplyr mutate
#' @importFrom rlang abort
#' @export
use_taxonomy <- function(
    df,
    kingdom = NULL,
    phylum = NULL,
    class = NULL,
    order = NULL,
    family = NULL,
    genus = NULL,
    species = NULL,
    specificEpiphet = NULL,
    vernacularName = NULL,
    .keep = "unused"
){
  if(missing(df)){
    abort("df is missing, with no default")
  }
  result <- df |>
    mutate(kingdom = {{kingdom}},
           phylum = {{phylum}},
           class = {{class}},
           order = {{order}},
           family = {{family}},
           genus = {{genus}},
           species = {{species}},
           specificEpiphet = {{specificEpiphet}},
           vernacularName = {{vernacularName}},
           .keep = .keep)
  check_kingdom(result, level = "abort")
  # check_phylum(result, level = "abort")
  # check_class(result, level = "abort")
  
  result
}