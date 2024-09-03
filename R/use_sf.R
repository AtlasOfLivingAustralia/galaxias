#' Add spatial fields to a `tibble` using `sf` `POINT` coordinates
#' 
#' This function helps format standard location fields to a `tibble`. It differs 
#' from `use_coordinates()` by accepting `sf` geometry columns of class `POINT` 
#' as coordinates (rather than `numeric` lat/lon coordinates). The advantage 
#' of using an `sf` geometry is that the Coordinate Reference System (CRS) is 
#' automatically formatted into the required `geodeticDatum` column.
#' 
#' @param .df a `data.frame` or `tibble` that the column should be appended to.
#' @param geometry the latitude/longitude coordinates in decimal degrees as `sf` `POINT` class
#' @param .keep Control which columns from .data are retained in the output. 
#' Note that unlike `dplyr::mutate`, which defaults to `"all"` this defaults to 
#' `"unused"`; i.e. only keeps Darwin Core fields, and not those fields used to 
#' generate them.
#' @returns A tibble with the requested fields added.
#' @seealso [use_coordinates()] for providing numeric coordinates, 
#' [use_locality()] for providing text-based spatial information
#' @importFrom rlang abort
#' @importFrom rlang get_expr
#' @importFrom sf st_drop_geometry
#' @importFrom sf st_geometry_type
#' @importFrom cli cli_warn
#' @importFrom cli cli_abort
#' @export
use_sf <- function(
    .df,
    geometry = NULL,
    .keep = "unused"
){
  if(missing(.df)){
    cli_abort(".df is missing, with no default.")
  }
  
  fn_args <- ls()
  
  # capture arguments as a list of quosures
  # NOTE: enquos() must be listed alphabetically
  fn_quos <- enquos(geometry)
  names(fn_quos) <- fn_args
  
  fn_quo_is_null <- fn_quos |> 
    purrr::map(\(user_arg)
               rlang::quo_is_null(user_arg)) |> 
    unlist()
  
  # detect sf and handle sf objects
  if (!inherits(.df, "sf")) {
    bullets <- c(
      "No geometry detected.",
      i = "Must supply {.code use_sf()} a dataframe with an {.pkg sf} geometry (i.e. {.code st_POINT})."
    ) |> cli_bullets() |> cli_fmt()
    cli_abort(bullets)
  } else {
    
    # enforce POINT geometry
    if (any(st_geometry_type(.df, by_geometry = FALSE) != "POINT")) {
      sf_type <- st_geometry_type(.df, by_geometry = FALSE)
      cli_abort(".df geometry must be of type 'POINT', not '{sf_type}'.")
      
    } else {
      
    # if geometry arg has been named, save the name
    if(!any(fn_quo_is_null)) {
      
      col_name_sfc <- paste0(get_expr(fn_quos$geometry)) # save name
      
      # check if column name is in the dataframe
      if(!col_name_sfc %in% colnames(.df)) {
        bullets <- c(
          "Must specify an existing 'geometry' column.",
          x = "Column '{col_name_sfc}' doesn't exist."
        ) |> cli_bullets() |> cli_fmt()
        
        cli_abort(bullets)
      } 
      
    } else {
      
      # get column name that holds 'geometry'
      col_name_sfc <- .df |>
        select(which(sapply(.df, class) == 'sfc_POINT')) |> # might be overcomplicating `select(geometry)`
        colnames()
    }
    }
  }
  
  # inform user which columns will be checked
  matched_cols <- col_name_sfc
  if(length(matched_cols > 0)) {
    col_progress_bar(cols = matched_cols)
  }
  
  # run column checks
  # Add sf coords if valid
  check_coords(.df, level = "abort")
  
  result <- col_sf_to_dwc(.df, col_name_sfc, level = level) |>
    st_drop_geometry()

  cli_warn("{.field {col_name_sfc}} dropped from data frame.")
  
  result
}

#' @rdname check_dwc
#' @order 6
#' @export
check_coords <- function(.df, 
                         level = c("inform", "warn", "abort")
){
  level <- match.arg(level)
    .df |>
      select(which(sapply(.df, class) == 'sfc_POINT')) |>
      check_is_sf(level = level) |>
      check_is_point(level = level) |>
      check_has_crs(level = level)
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
  if (!inherits(.df, "sf")) {
    bullets <- c(
      "No geometry detected.",
      i = "Must supply {.code use_sf()} a dataframe with an {.pkg sf} geometry (i.e. {.code st_POINT})."
    ) |> cli_bullets() |> cli_fmt()
    cli_abort(bullets)
  }
  .df
}

#' @rdname check_dwc
#' @order 6
#' @importFrom sf st_is
#' @importFrom sf st_geometry
#' @export
check_is_point <- function(.df, 
                        level = c("inform", "warn", "abort"),
                        call = caller_env()
){
  check_data_frame(.df)
  # enforce POINT geometry
  if (any(st_geometry_type(.df, by_geometry = FALSE) != "POINT")) {
    sf_type <- st_geometry_type(.df, by_geometry = FALSE)
    cli_abort(".df geometry must be of type 'POINT', not '{sf_type}'.")
    
  }
  .df
}

#' @rdname check_dwc
#' @order 6
#' @importFrom sf st_crs
#' @importFrom sf st_geometry
#' @importFrom cli cli_bullets
#' @importFrom cli cli_fmt
#' @export
check_has_crs <- function(.df, 
                          level = c("inform", "warn", "abort"),
                          call = caller_env()
){
  check_data_frame(.df)
  if(is.na(st_crs(.df))){
    
    bullets <- cli_bullets(c(
      "Missing Coordinate Reference System (CRS).",
      i = ".df must have CRS. See {.code ?sf::st_crs}."
    )) |>
      cli_fmt()
    
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
#' @importFrom cli cli_bullets
#' @importFrom cli cli_fmt
#' @keywords Internal
col_sf_to_dwc <- function(.df, 
                          col_name,
                          level = c("inform", "warn", "abort"),
                          call = caller_env()
                          ){
  result <- .df |>
    mutate(
      decimalLongitude = sf::st_coordinates(.df)[,1],
      decimalLatitude = sf::st_coordinates(.df)[,2],
      geodeticDatum = sf::st_crs(.df)$input
      )
  
  new_cols <- colnames(result)[colnames(result) %in% c("decimalLongitude", "decimalLatitude", "geodeticDatum")]
  bullets <- c(
    "*" = "Converted {cli::col_cyan(paste('geometry'))} {symbol$arrow_right} {.field {new_cols}}."
    ) |> cli_bullets() |> cli_fmt()
  
  cli_inform(bullets)
  return(result)
  
}
