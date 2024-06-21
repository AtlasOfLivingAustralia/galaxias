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
  
  check_missing_args(match.call(), ls())
  
  result <- df |>
    mutate(continent = {{continent}},
           country = {{country}},
           countryCode = {{countryCode}},
           stateProvince = {{stateProvince}},
           locality = {{locality}},
           .keep = .keep)
  check_continent(result, level = "abort")
  check_country(result, level = "abort")
  check_countryCode(result, level = "abort")
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
    check_contains(unique(x), 
                   accepted_values, 
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
      check_is_string(x, level = level)
    
    #TODO: This is broken
    # missing value argument
    accepted_values <- country_codes$code
    if(!(value %in% accepted_values)){
      bullets <- c("Unexpected value in {.field countryCode}.", 
                   i = "Please provide a two-digit country code in ISO 3166-1 Alpha 2",
                   i = "See {.url https://en.wikipedia.org/wiki/ISO_3166-1_alpha-2} for country codes.",
                   x = "Invalid value{?s}: {value}"
                   )
      cli::cli_abort(bullets, call = call)
    }
    
    if(any(colnames(df) == "countryCode")){
      if(!(df$countryCode[1] %in% country_codes$country_name)){
        bullets <- c("Unrecognised {.field countryCode} value.",
                     # i = "Did you mean X?",
                     x = "Did not recognise: {df$countryCode}.")
        cli::cli_warn(bullets)
      }
      lookup_country <- country_codes$country_name[country_codes$code == value]
      if(lookup_country != df$countryCode[1]){
        bullets <- c("Value supplied in {.field countryCode} does not correspond to supplied country code",
                     i = "Did you mean X?"
                     )
        cli::cli_warn(bullets)
      }
    }
  }
}