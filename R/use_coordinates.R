#' Add spatial fields to a `tibble`
#' 
#' This function helps format standard location fields to a `tibble`. In 
#' practice this is no different from using `mutate()`, but gives some 
#' informative errors, and serves as a useful lookup for how spatial fields are
#' represented in the Darwin Core Standard.
#' @param df a `data.frame` or `tibble` that the column should be appended to.
#' @param decimalLatitude the latitude in decimal degrees
#' @param decimalLongitude the longitude in decimal degrees
#' @param geodeticDatum the datum that coordinates are recorded against
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
#' * `geodeticDatum` should be a valid EPSG code
#' * `coordinatePrecision` should be no less than 0.00001 if data were collected
#' using GPS
#' * `coordinateUncertaintyInMeters` will typically be around `30` (metres) if
#' recorded with a GPS after 2000, or `100` before that year. 
#' @seealso [use_locality()] for provided text-based spatial information
#' @importFrom dplyr mutate
#' @importFrom rlang abort
#' @export
use_coordinates <- function(
    df,
    decimalLatitude = NULL,
    decimalLongitude = NULL,
    geodeticDatum = NULL,
    coordinateUncertaintyInMeters = NULL,
    coordinatePrecision = NULL,
    .keep = "unused"
){
  if(missing(df)){
    abort("df is missing, with no default")
  }
  result <- df |>
    mutate(decimalLatitude = decimalLatitude,
           decimalLongitude = decimalLongitude,
           geodeticDatum = geodeticDatum,
           coordinateUncertaintyInMeters = coordinateUncertaintyInMeters,
           coordinatePrecision = coordinatePrecision,
           .keep = .keep)
  check_decimalLatitude(df, level = "abort")
  check_decimalLongitude(df, level = "abort")
  # other tests likely to be needed here
  result
}


#' @rdname check_dwc
#' @order 6
#' @export
check_decimalLatitude <- function(df, 
                                  level = c("inform", "warn", "abort")
){
  level <- match.arg(level)
  x <- df$decimalLatitude
  check_is_numeric(x, 
                   level = level)
  check_within_range(x, 
                     level = level, 
                     lower = -90, 
                     upper = 90)
}

#' @rdname check_dwc
#' @order 7
#' @export
check_decimalLongitude <- function(df, 
                                   level = c("inform", "warn", "abort")
){
  level <- match.arg(level)
  x <- df$check_decimalLongitude
  check_is_numeric(x, 
                   level = level)
  check_within_range(x, 
                     level = level, 
                     lower = -180, 
                     upper = 180)
}