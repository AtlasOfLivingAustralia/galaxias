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
    .df,
    recordedBy = NULL,
    recordedByID = NULL,
    .keep = "unused"
){
  if(missing(.df)){
    abort("df is missing, with no default")
  }
  
  fn_args <- ls()
  
  # capture arguments as a list of quosures
  # NOTE: enquos() must be listed alphabetically
  fn_quos <- enquos(recordedBy, recordedByID)
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
  col_progress_bar(cols = matched_cols)
  
  # run column checks
  check_recordedBy(level = "abort")
  check_recordedByID(level = "abort")
  
  result
}

#' Check recordedBy field is valid
#' 
#' @rdname check_dwc
#' @param level what action should the function take for non-conformance? 
#' Defaults to `"inform"`.
#' @order 7
#' @export
check_recordedBy <- function(.df, 
                             level = c("inform", "warn", "abort")
){
  level <- match.arg(level)
  if(any(colnames(.df) == "recordedBy")){
    .df |>
      select("recordedBy") |>
      check_is_string(level = level)
  }
  .df
}

#' Check recordedByID field is valid
#' 
#' @rdname check_dwc
#' @param level what action should the function take for non-conformance? 
#' Defaults to `"inform"`.
#' @order 7
#' @export
check_recordedByID <- function(.df, 
                             level = c("inform", "warn", "abort")
){
  level <- match.arg(level)
  if(any(colnames(.df) == "recordedByID")){
    .df |>
      select("recordedByID") |>
      check_is_string(level = level)
    # could check if contains "orchid.org"?
  }
  .df
}