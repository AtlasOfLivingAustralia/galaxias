#' Provide configuration information to `galaxias`
#' 
#' To validate (or in future, to publish) your dataset, you need to provide
#' credentials to the relevant web service. This function allows you to store
#' that information for access by `galaxias` API functions.
#' @details
#' Note that unlike `galah`, you cannot set a 'default' provider in `galaxias`;
#' the organisation is always an argument to the function in question. Also 
#' unlike `galah`, `galaxias_config()` enables you to store configuration 
#' details for multiple organisations at once. Currently, this function is
#' only useful to `validate_archive()`, and only then for validating
#' via GBIF.
#' @name galaxias_config 
#' @param gbif A list containing the entries `username`, `email` and `password`
#' @importFrom potions brew
#' @importFrom potions pour
#' @export
galaxias_config <- function(gbif){
  # check if all arguments are missing
  all_missing <- c(
    missing(gbif)
    # missing(ala) # etc
  ) |>
    all()
  if(all_missing){ # if so, see whether data are already cached
    if(length(pour()) < 1) { # if no caching, set defaults
      galaxias_default_config() |>
        brew()
      pour()
    }else{ # if something is cached, return it
      pour()
    }
  }else{ # if arguments are supplied, store them
    if(!missing(gbif)){
      check_gbif_credentials(gbif)
      brew(gbif = gbif, method = "leaves")
    } # NOTE: no `else` required here, as that is handled above
    # that will change once >1 service/organisation added
  }
}

#' Check a list contains only strings
#' @importFrom purrr map
#' @importFrom rlang abort
#' @noRd
#' @keywords Internal
check_gbif_credentials <- function(x){
  
  # check is a list
  if(!is.list(x)){
    abort("GBIF credentials should be supplied as a `list`.")
  }
  
  # check all names are supplied, and only those names
  if(!(all(names(x) %in% c("username", "email", "password")) &
       all(c("username", "email", "password") %in% names(x)))){
   abort("GBIF credentials should be named `username`, `email` and `password`.")
  }
  
  # check list only contains characters
  character_check <- map(x, is.character) |>
    unlist() |>
    all()
  if(!character_check){
    abort("All GBIF credentials should be supplied as strings.")
  }
  
  # check all entries are length 1
  length_check <- all(lengths(x) == 1L)
  if(!length_check){
    abort("All GBIF credentials should be length-1.")
  }
}

#' Set a 'default' object for storing config in `galah`
#' @noRd
#' @keywords Internal
galaxias_default_config <- function(){
  x <- list(
    gbif = list(username = "",
                email = "",
                password = "")
    # ala = list()
  )
  class(x) <- c("galaxias_config", "list")
  x
}