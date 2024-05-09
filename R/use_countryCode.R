#' Add a `countryCode` field to a `tibble`
#' 
#' `countryCode` is a Darwin Core field that defines the country of observation.
#' [TDWG recommend](https://dwc.tdwg.org/list/#dwc_countryCode) that 
#' `countryCode` should be supplied according to the 
#' [ISO 3166-1 ALPHA-2](https://en.wikipedia.org/wiki/ISO_3166-1_alpha-2) 
#' standard, and that is enforced here.
#' @param df a `data.frame` or `tibble` that the column should be appended to.
#' @param value what value should this field take? Only accepts camelCase, for
#' consistency with field names.
#' @returns A tibble with an attached `countryCode` field
#' @details
#' Currently only accepts a single argument. Could probably be made more 
#' flexible. Also checks whether `country` field is present, and if so, 
#' whether its' contents match those given in `value`; if not returns a
#' `warning`.
#' @importFrom dplyr mutate
#' @importFrom rlang abort
#' @importFrom rlang warn
#' @export
use_countryCode <- function(df, 
                            value = NULL){
  if(missing(df)){
    abort("df is missing, with no default")
  }
  
  if(is.null(value)){
    bullets <- c("`value` is missing, with no default",
                 i = "Please provide a two-digit country code in ISO 3166-1 Alpha 2",
                 i = "see https://en.wikipedia.org/wiki/ISO_3166-1_alpha-2")
    abort(bullets)
  }
  
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
  
  .df |>
    mutate(countryCode = value)
}
# Q: Add function to show countryCodes?