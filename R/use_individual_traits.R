#' Add information of individual organisms to a `tibble`
#' 
#' Format fields that contain measurements or attributes of individual 
#' organisms to a `tibble`. Fields include those that specify sex, life stage 
#' or condition. Individuals are identified by an `individualID`.
#' 
#' In practice this is no different from using `mutate()`, but gives some 
#' informative errors, and serves as a useful lookup for fields in 
#' the Darwin Core Standard.
#' @param df a `data.frame` or `tibble` that the column should be appended to.
#' @param individualID An identifier for an individual or named group of 
#' individual organisms represented in the Occurrence. Meant to accommodate 
#' resampling of the same individual or group for monitoring purposes. May 
#' be a global unique identifier or an identifier specific to a data set.
#' @param lifeStage The age class or life stage of organism at the time of 
#' occurrence.
#' @param sex The sex of the biological individual.
#' @param vitality An indication of whether an organism was alive or dead at 
#' the time of collection or observation.
#' @param reproductiveCondition The reproductive condition of the biological 
#' individual.
#' @param .keep Control which columns from .data are retained in the output. 
#' Note that unlike `dplyr::mutate`, which defaults to `"all"` this defaults to 
#' `"unused"`; i.e. only keeps Darwin Core fields, and not those fields used to 
#' generate them.
#' @returns A tibble with the requested fields added.
#' @details
#' Examples of `lifeStage` values:
#' * `zygote`
#' * `larva`
#' * `adult`
#' * `seedling`
#' * `flowering`
#' 
#' Examples of `vitality` values:
#' * `alive`
#' * `dead`
#' * `uncertain`
#' 
#' Examples of `reproductiveCondition` values:
#' * `non-reproductive`
#' * `pregnant`
#' * `in bloom`
#' * `fruit bearing`
#' 
#' @seealso [use_scientific_name()] for adding `scientificName` and authorship information.
#' @importFrom dplyr mutate
#' @importFrom rlang abort
#' @export
use_individual_traits <- function(
    df,
    individualID = NULL,
    lifeStage = NULL,
    sex = NULL,
    vitality = NULL,
    reproductiveCondition = NULL,
    .keep = "unused"
){
  if(missing(df)){
    abort("df is missing, with no default")
  }
  result <- df |>
    mutate(individualID = {{individualID}},
           lifeStage = {{lifeStage}},
           sex = {{sex}},
           vitality = {{vitality}},
           reproductiveCondition = {{reproductiveCondition}},
           .keep = .keep)
  check_individualID(result, level = "abort")
  # check_lifeStage(result, level = "abort")
  # check_sex(result, level = "abort")
  
  result
}