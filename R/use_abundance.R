#' Add abundance fields to a `tibble`
#' 
#' In some field methods, it is common to observe more than one individual
#' per observation; to observe abundance using non-integer measures such as 
#' mass or area; or to seek individuals but not find them (abundance of zero). 
#' As these approaches use different DwC terms, this function assists in 
#' specifying abundances in a flexible way.
#' @param .df a `data.frame` or `tibble` that the column should be appended to.
#' @param individualCount The number of individuals present
#' @param organismQuantity A number or enumeration value for the quantity
#' @param organismQuantityType The type of quantification system used for `organismQuantity`
#' @param occurrenceStatus whether the taxon in question is `present` or `absent`
#' @returns A tibble with the requested fields (see details).
#' @details
#' This needs some clever behaviour
#' @importFrom dplyr mutate
#' @importFrom rlang abort
#' @noRd
add_abundance <- function(.df,
                          individualCount = NULL,
                          organismQuantity = NULL,
                          organismQuantityType = NULL,
                          occurrenceStatus = NULL
                          ){
  if(missing(.df)){
    abort("df is missing, with no default")
  }
  
  fn_args <- ls()
  
  # capture arguments as a list of quosures
  # NOTE: enquos() must be listed alphabetically
  fn_quos <- enquos(individualCount, occurrenceStatus, organismQuantity, organismQuantityType)
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
  
  check_missing_all_args(match.call(), fn_args)
  
  # Update df
  result <- .df |> 
    mutate(!!!fn_quos, 
           .keep = .keep)
  
  # inform user which columns will be checked
  matched_cols <- names(result)[names(result) %in% fn_args]
  col_check_spinny_message(cols = matched_cols)
  
  # run column checks
  # TODO: Add checks
  
  return(result)
  
}

#' Check individualCount field is valid
#' @rdname check_dwc
#' @param level what action should the function take for non-conformance? 
#' Defaults to `"inform"`.
#' @order 4
#' @export
check_individualCount <- function(.df, 
                                  level = c("inform", "warn", "abort")
                                  ){
  level <- match.arg(level)
  if(any(colnames(.df) == "basisOfRecord")){
    .df |>
      select("basisOfRecord") |>
      check_is_numeric(level = level)
  }
  .df
}