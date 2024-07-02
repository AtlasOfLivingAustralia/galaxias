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
  check_missing_all_args(match.call(), fn_args)
  
  # capture arguments as a list of quosures
  # NOTE: enquos() must be listed alphabetically
  fn_quos <- enquos(individualCount, occurrenceStatus, organismQuantity, organismQuantityType)
  names(fn_quos) <- fn_args
  
  # find arguments that are NULL but exist already in `df`
  # these DwC columns are otherwise deleted by `mutate()` later
  fn_quo_is_null <- fn_quos |> 
    purrr::map(\(user_arg)
               rlang::quo_is_null(user_arg)) |> 
    unlist()
  
  null_col_exists_in_df <- fn_quo_is_null & (names(fn_quos) %in% colnames(.df))
  
  if(any(null_col_exists_in_df)){
    purrr::pluck(fn_quos, names(which(null_col_exists_in_df))) <- rlang::zap()
  }
  
  # Update df
  result <- .df |> 
    mutate(!!!fn_quos, 
           .keep = .keep)
  
  
  
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