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
    .df,
    scientificName = NULL,
    scientificNameRank = NULL,
    scientificNameAuthorship = NULL,
    .keep = "unused"
){
  if(missing(.df)){
    abort(".df is missing, with no default")
  }
  
  fn_args <- ls()
  
  # capture arguments as a list of quosures
  # NOTE: enquos() must be listed alphabetically
  fn_quos <- enquos(scientificName, scientificNameAuthorship, scientificNameRank)
  names(fn_quos) <- fn_args
  
  # find arguments that are NULL but exist already in `df`
  # then remove their names before `mutate()`
  # otherwise, these DwC columns are deleted by `mutate(.keep = "unused")` 
  fn_quo_is_null <- fn_quos |> 
    purrr::map(\(user_arg)
               rlang::quo_is_null(user_arg)) |> 
    unlist()
  
  null_col_exists_in_df <- fn_quo_is_null & (names(fn_quos) %in% colnames(.df))
  
  if(any(null_col_exists_in_df)){
    fn_quos <- fn_quos |> 
      purrr::keep(!names(fn_quos) %in% names(which(null_col_exists_in_df)))
  }
  
  # Update df
  result <- .df |> 
    mutate(!!!fn_quos, 
           .keep = .keep)
  
  check_missing_all_args(fn_call = match.call(), 
                         fn_args = fn_args, 
                         user_cols = colnames(result))
  
  # inform user which columns will be checked
  matched_cols <- names(result)[names(result) %in% fn_args]
  if(length(matched_cols > 0)) {
    col_progress_bar(cols = matched_cols)
  }
  
  # run column checks
  check_scientificName(result, level = "abort")
  check_scientificNameRank(result, level = "abort")
  check_scientificNameAuthorship(result, level = "abort")

  result
}

#' Check scientificName field is valid
#' 
#' @rdname check_dwc
#' @param level what action should the function take for non-conformance? 
#' Defaults to `"inform"`.
#' @order 7
#' @export
check_scientificName <- function(.df, 
                                 level = c("inform", "warn", "abort")
){
  level <- match.arg(level)
  if(any(colnames(.df) == "scientificName")){
    .df |>
      select("scientificName") |>
      check_is_string(level = level)
  }
  .df
}
# TODO: Currently only checks whether input is a string

#' Check scientificNameRank field is valid
#' 
#' @rdname check_dwc
#' @param level what action should the function take for non-conformance? 
#' Defaults to `"inform"`.
#' @order 7
#' @export
check_scientificNameRank <- function(.df, 
                                     level = c("inform", "warn", "abort")
){
  level <- match.arg(level)
  if(any(colnames(.df) == "scientificNameRank")){
    .df |>
      select("scientificNameRank") |>
      check_is_string(level = level)
    # Should this check a list of valid values?
  }
  .df
}
# TODO: Currently only checks whether input is a string

#' Check scientificNameAuthorship field is valid
#' 
#' @rdname check_dwc
#' @param level what action should the function take for non-conformance? 
#' Defaults to `"inform"`.
#' @order 7
#' @export
check_scientificNameAuthorship <- function(.df, 
                                           level = c("inform", "warn", "abort")
){
  level <- match.arg(level)
  if(any(colnames(.df) == "scientificNameAuthorship")){
    .df |>
      select("scientificNameAuthorship") |>
      check_is_string(level = level)
  }
  .df
}
