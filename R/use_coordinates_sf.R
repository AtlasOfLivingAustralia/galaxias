#' Add spatial fields to a `tibble` using `sf` `POINT` coordinates
#' 
#' This function helps format standard location fields to a `tibble`. It differs 
#' from `use_coordinates()` by accepting `sf` geometry columns of class `POINT` 
#' as coordinates (rather than `numeric` lat/lon coordinates). The advantage 
#' of using an `sf` geometry is that the Coordinate Reference System (CRS) is 
#' automatically formatted into the required `geodeticDatum` column.
#' 
#' In practice this is no different from using `mutate()`, but gives some 
#' informative errors, and serves as a useful lookup for how spatial fields are
#' represented in the Darwin Core Standard.
#' @param df a `data.frame` or `tibble` that the column should be appended to.
#' @param coords the latitude in decimal degrees as `sf` `POINT` class
#' @param coordinateUncertaintyInMeters (numeric) radius of the smallest circle 
#' that contains the whole location, given any possible measurement error.
#' @param coordinatePrecision (numeric) the precision that `decimalLatitude` and 
#' `decimalLongitude` are supplied to.
#' @param .keep Control which columns from .data are retained in the output. 
#' Note that unlike `dplyr::mutate`, which defaults to `"all"` this defaults to 
#' `"unused"`; i.e. only keeps Darwin Core fields, and not those fields used to 
#' generate them.
#' @returns A tibble with the requested fields added.
#' @details
#' Example values are:
#' * `coordinatePrecision` should be no less than 0.00001 if data were collected
#' using GPS
#' * `coordinateUncertaintyInMeters` will typically be around `30` (metres) if
#' recorded with a GPS after 2000, or `100` before that year. 
#' @seealso [use_locality()] for provided text-based spatial information
#' @importFrom dplyr mutate
#' @importFrom rlang abort
#' @importFrom sf st_drop_geometry
#' @export
use_coordinates_sf <- function(
    .df,
    coords = NULL,
    coordinateUncertaintyInMeters = NULL,
    coordinatePrecision = NULL,
    .keep = "unused"
){
  if(missing(.df)){
    abort(".df is missing, with no default.")
  }
  
  fn_args <- ls()
  
  # capture arguments as a list of quosures
  # NOTE: enquos() must be listed alphabetically
  fn_quos <- enquos(coords, coordinatePrecision, coordinateUncertaintyInMeters)
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
  
  # browser()
  # handle sf object operations prior to `mutate()`
  if (inherits(.df, "sf")) {
    # get `geometry` column name
    col_name_sfc <- .df |>
      select(which(sapply(.df, class) == 'sfc_POINT')) |>
      colnames()
    
    coords <- .df |>
      select(which(sapply(.df, class) == 'sfc_POINT'))
    # st_geometry(.df) <- "coords"
    fn_quos <- fn_quos |>
      purrr::keep(!names(fn_quos) %in% "coords")
  } else {
    bullets <- c(
      "No {.code geometry} detected.",
      i = "{.code use_coordinates_sf()} must be supplied a dataframe with an {.pkg sf} geometry (i.e. {.code st_POINT})."
    ) |> cli_bullets() |> cli_fmt()
    cli::cli_abort(bullets)
  }
  
  # Update df
  result <- .df |> 
    mutate(!!!fn_quos, 
           .keep = .keep)
  
  # browser()
  # unique to this use_coordinates_sf, `geometry` is a valid term
  fn_args <- c(fn_args, col_name_sfc)
  
  check_missing_all_args(fn_call = match.call(), 
                         fn_args = fn_args, 
                         user_cols = colnames(result))
  
  # inform user which columns will be checked
  matched_cols <- names(result)[names(result) %in% fn_args]
  if(length(matched_cols > 0)) {
    col_progress_bar(cols = matched_cols)
  }
  
  # run column checks
  # Add sf coords if valid
  check_coords(result, level = "abort")
  
  result <- col_sf_to_dwc(.df, level = level) |>
    st_drop_geometry()

  cli::cli_warn("{.field {col_name_sfc}} dropped from data frame.")
  
  result
}

#' @rdname check_dwc
#' @order 6
#' @export
check_coords <- function(.df, 
                         level = c("inform", "warn", "abort")
){
  level <- match.arg(level)
  if(any(colnames(.df) == "coords")){
    .df |>
      select("coords") |>
      check_is_sf(level = level)
      # check_has_crs(level = level)
  } 
}

#' @rdname check_dwc
#' @order 6
#' @importFrom sf st_is
#' @importFrom sf st_geometry
#' @export
check_is_sf <- function(.df, 
                        level = c("inform", "warn", "abort"),
                        call = caller_env()
){
  check_data_frame(.df)
  # field_name <- colnames(.df)[[1]]
  # x <- .df |> pull(field_name)
  if(!all(st_is(st_geometry(df), "POINT"))){
    
    bullets <- cli::cli_bullets(c(
      "{.field {field_name}} must be a POINT geometry, not {class(x)}."
    )) |>
      cli::cli_fmt()
    
    switch_check(level,
                 bullets,
                 call = call)
  }
  .df
}

#' @rdname check_dwc
#' @order 6
#' @importFrom sf st_coordinates
#' @importFrom sf st_crs
#' @importFrom sf st_drop_geometry
#' @keywords Internal
col_sf_to_dwc <- function(.df, 
                          level = c("inform", "warn", "abort"),
                          call = caller_env()
                          ){
  # check_data_frame(.df)
  # field_name <- colnames(.df)[[1]]
  # x <- .df |> pull(field_name)
  result <- .df |>
    mutate(
      decimalLongitude = sf::st_coordinates(.df)[,1],
      decimalLatitude = sf::st_coordinates(.df)[,2],
      geodeticDatum = sf::st_crs(.df)$input
      )
  
  new_cols <- colnames(result)[colnames(result) %in% c("decimalLongitude", "decimalLatitude", "geodeticDatum")]
  bullets <- c("*" = "Converted {cli::col_cyan(paste('coords'))} {symbol$arrow_right} {.field {new_cols}}.") |> cli_bullets() |> cli_fmt()
  cli_inform(bullets)
  
  return(result)
  
}
