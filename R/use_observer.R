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
#' @importFrom rlang quo_is_null
#' @importFrom rlang enquos
#' @importFrom rlang zap
#' @importFrom purrr map
#' @importFrom purrr pluck
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
  fn_args <- ls()
  check_missing_all_args(match.call(), fn_args)
  
  # capture arguments as a list of quosures
  # NOTE: This stage is a bit manual rn, could generalise by capturing supplied
  # argument names and removing `df` and `.keep`.
  fn_quos <- enquos(continent, country, countryCode, locality, stateProvince)
  names(fn_quos) <- c("continent", "country", "countryCode", "locality", "stateProvince") 
  # NOTE: this works as an alternative to above, but only if enquos() are listed alphabetically
  # names(x) <- fn_args[!fn_args %in% "df"] 
  
  # check for NULL arguments
  fn_quo_is_null <- fn_quos |> 
    purrr::map(\(user_arg)
        rlang::quo_is_null(user_arg)) |> 
    unlist()
  
  # find any arguments that are NULL, but exist already in `df`
  #   (if not handled here, these DwC columns would be deleted by `mutate()`)
  null_col_exists_in_df <- fn_quo_is_null & (names(fn_quos) %in% colnames(df))
  
  if(any(null_arg_nonnull_df)){
    purrr::pluck(fn_quos, names(which(null_col_exists_in_df))) <- rlang::zap()
  }
  
  # Update df
  result <- df |> 
    mutate(!!!fn_quos, 
           .keep = .keep)
}