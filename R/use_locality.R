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
#' @export
use_locality <- function(df, 
                         continent = NULL,
                         country = NULL,
                         countryCode = NULL,
                         stateProvince = NULL,
                         locality = NULL,
                         .keep = "unused"
){
  if(missing(df)){
    abort("df is missing, with no default")
  }
  result <- df |>
    mutate(continent = continent,
           country = country,
           countryCode = countryCode,
           stateProvince = stateProvince,
           locality = locality,
           .keep = .keep)
  check_continent(df, level = "abort")
  check_country(df, level = "abort")
  check_countryCode(df, level = "abort")
  if(!is.null(stateProvince)){check_is_string(df$stateProvince)}
  if(!is.null(locality)){check_is_string(df$locality)}
  # other tests likely to be needed here
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
  if(any(colnames(df) == "continent")){
    x <- df$continent
    check_is_string(x)
    accepted_values <- c("Africa",
                         "Antarctica",
                         "Asia",
                         "Europe",
                         "North America",
                         "Oceania",
                         "South America")
    check_contains(unique(x), accepted_values)
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
    check_is_string(df$country)
    check_contains(unique(df$country), 
                   country_codes$country_name)
  }
}


#' @rdname check_dwc
#' @order 5
#' @export
check_countryCode <- function(df, 
                              level = c("inform", "warn", "abort")
){
  level <- match.arg(level)
  if(any(colnames(df) == "countryCode")){
    x <- df$countryCode
    check_is_string(x)
    
    accepted_values <- country_codes$code
    if(!(value %in% accepted_values)){
      bullets <- c("Unexpected `value` received", 
                   i = "Please provide a two-digit country code in ISO 3166-1 Alpha 2",
                   i = "see https://en.wikipedia.org/wiki/ISO_3166-1_alpha-2")
      abort(bullets)
    }
    
    if(any(colnames(df) == "country")){
      if(!(df$country[1] %in% country_codes$country_name)){
        bullets <- c("supplied `df` has a `country` field that is not present in the reference dataset",
                     i = "did you mean X?")
        warn(bullets)
      }
      lookup_country <- country_codes$country_name[country_codes$code == value]
      if(lookup_country != df$country[1]){
        bullets <- c("supplied `df` has a `country` field that does not correspond to the supplied country code",
                     i = "did you mean X?")
        warn(bullets)
      }
    }
  }
}