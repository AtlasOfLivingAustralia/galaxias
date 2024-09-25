#' Add `locality` data to a `tibble`
#' 
#' Locality information refers to a description of a place, rather than a 
#' spatial coordinate. 
#' @param df a `data.frame` or `tibble` that the column should be appended to.
#' @param continent (string) Valid continent. See details.
#' @param country Valid country name. See `country_codes`.
#' @param countryCode Valid country code. See `country_codes`.
#' @param stateProvince A sub-national region.
#' @param locality A specific location, such as a property or address.
#' @param .keep Control which columns from .data are retained in the output. 
#' Note that unlike `dplyr::mutate`, which defaults to `"all"` this defaults to 
#' `"unused"`; i.e. only keeps Darwin Core fields, and not those fields used to 
#' generate them.
#' @returns A tibble with the requested fields.
#' @details
#' Example values are:
#' * `continent` should be one of `"Africa"`, `"Antarctica"`, `"Asia"`, 
#' `"Europe"`, `"North America"`, `"Oceania"` or `"South America"`.
#' * `countryCode` should be supplied according to the 
#' [ISO 3166-1 ALPHA-2](https://en.wikipedia.org/wiki/ISO_3166-1_alpha-2) 
#' standard, [as per TDWG advice](https://dwc.tdwg.org/list/#dwc_countryCode).
#' @seealso [use_coordinates()] for numeric spatial data
#' @importFrom dplyr mutate
#' @importFrom rlang abort
#' @importFrom rlang warn
#' @importFrom rlang enquos
#' @importFrom rlang quo_is_null
#' @importFrom rlang zap
#' @importFrom purrr map
#' @importFrom purrr pluck
#' @export
use_locality <- function(.df, 
                         continent = NULL,
                         country = NULL,
                         countryCode = NULL,
                         stateProvince = NULL,
                         locality = NULL,
                         .keep = "unused"
){
  if(missing(.df)){
    abort("df is missing, with no default")
  }
  
  fn_args <- ls()
  
  # capture arguments as a list of quosures
  # NOTE: enquos() must be listed alphabetically
  fn_quos <- enquos(continent, country, countryCode, locality, stateProvince)
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
  check_continent(result, level = "abort")
  check_country(result, level = "abort")
  check_countryCode(result, level = "abort")
  check_stateProvince(result, level = "abort")
  check_locality(result, level = "abort")
  
  result
}

# Q: Add function to show countryCodes?


#' @rdname check_dwc
#' @order 5
#' @export
check_continent <- function(df, 
                            level = c("inform", "warn", "abort")
                            ){
  level <- match.arg(level)
  accepted_values <- c("Africa",
                       "Antarctica",
                       "Asia",
                       "Europe",
                       "North America",
                       "Oceania",
                       "South America")
  
  if(any(colnames(df) == "continent")){
    df |>
      select("continent") |>
      check_is_string() |>
      check_contains_values(accepted_values, 
                            level = level)
  }
}


#' @rdname check_dwc
#' @order 5
#' @export
check_country <- function(df,
                          level = c("inform", "warn", "abort")
){
  level <- match.arg(level)
  if(any(colnames(df) == "country")){
    df |>
      select("country") |>
      check_is_string(level = level) |>
      check_contains_values(country_codes$country_name, 
                            level = level,
                            .accepted_message = FALSE)
  }
}


#' @rdname check_dwc
#' @order 5
#' @export
check_countryCode <- function(df, 
                              level = c("inform", "warn", "abort"),
                              call = caller_env()
){
  level <- match.arg(level)
  if(any(colnames(df) == "countryCode")){
    df |> 
      select("countryCode") |>
      check_is_string(level = level) |>
      check_contains_values(country_codes$code,
                            level = level,
                            .accepted_message = FALSE)
  }
  # browser()
  # FIXME: Check that country code matches country
  # NOTE: Does this need to be in galaxias?
  # if(any(colnames(df) == "country")) {
  #   df |>
  #     select("countryCode", "country") |>
  #     check_mismatch_code_country(level = level)
  # }
}


#' Check stateProvince field is valid
#' 
#' @rdname check_dwc
#' @param level what action should the function take for non-conformance? 
#' Defaults to `"inform"`.
#' @order 5
#' @export
check_stateProvince <- function(.df, 
                                 level = c("inform", "warn", "abort")
){
  level <- match.arg(level)
  if(any(colnames(.df) == "stateProvince")){
    .df |>
      select("stateProvince") |>
      check_is_string(level = level)
  }
  .df
}

#' Check locality field is valid
#' 
#' @rdname check_dwc
#' @param level what action should the function take for non-conformance? 
#' Defaults to `"inform"`.
#' @order 5
#' @export
check_locality <- function(.df, 
                           level = c("inform", "warn", "abort")
){
  level <- match.arg(level)
  if(any(colnames(.df) == "locality")){
    .df |>
      select("locality") |>
      check_is_string(level = level)
  }
  .df
}