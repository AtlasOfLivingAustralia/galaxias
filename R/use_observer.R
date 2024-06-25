#' Add who made an observation to a `tibble`
#' 
#' Format fields that contain information about who made a specific observation 
#' of an organism.
#' 
#' In practice this is no different from using `mutate()`, but gives some 
#' informative errors, and serves as a useful lookup for fields in 
#' the Darwin Core Standard.
#' @param df a `data.frame` or `tibble` that the column should be appended to.
#' @param recordedBy Names of people, groups, or organizations responsible for 
#' recording the original occurrence. The primary collector or observer should 
#' be listed first.
#' @param recordedByID The globally unique identifier for the person, people, 
#' groups, or organizations responsible for recording the original occurrence.
#' @param .keep Control which columns from .data are retained in the output. 
#' Note that unlike `dplyr::mutate`, which defaults to `"all"` this defaults to 
#' `"unused"`; i.e. only keeps Darwin Core fields, and not those fields used to 
#' generate them.
#' @returns A tibble with the requested fields added.
#' @details
#' Examples of `recordedBy` values:
#' * `José E. Crespo`
#' 
#' Examples of `recordedByID` values:
#' * 	`c("https://orcid.org/0000-0002-1825-0097", "https://orcid.org/0000-0002-1825-0098")`
#' 
#' @importFrom dplyr mutate
#' @importFrom purrr map
#' @importFrom purrr pluck
#' @importFrom rlang abort
#' @importFrom rlang as_label
#' @importFrom rlang enquos
#' @importFrom rlang zap
#' @export
use_observer <- function(
    df,
    recordedBy = NULL,
    recordedByID = NULL,
    .keep = "unused"
){
  if(missing(df)){
    abort("df is missing, with no default")
  }
  # capture arguments as a list of quosures
  # NOTE: This stage is a bit manual rn, could generalise by capturing supplied
  # argument names and removing `df` and `.keep`.
  x <- enquos(recordedBy, recordedByID)
  names(x) <- c("recordedBy", "recordedByID") 
  
  # check for NULLs
  x_null <- map(x, \(a){as_label(a) == "NULL"}) |> unlist()
  
  # find any arguments that are supplied as `NULL`, but are already given in `df`
  # if not handled here, these columns would be deleted by `mutate()`,
  # which is undesirable as they already conform to DwC.
  null_arg_nonnull_df <- x_null & (names(x) %in% colnames(df))
  if(any(null_arg_nonnull_df)){
    pluck(x, names(which(null_arg_nonnull_df))) <- zap()
  }
  
  # pass list to `mutate`
  mutate(df, !!!x, .keep = .keep)
}