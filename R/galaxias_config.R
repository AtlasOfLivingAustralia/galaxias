#' Provide configuration information to `galaxias`
#' 
#' To validate (or in future, to publish) your dataset, you need to provide
#' credentials to the relevant web service. This function allows you to store
#' that information for access by `galaxias` API functions. This function 
#' also enables you to change the directory where working documents are stored,
#' which defaults to `data-publish`.
#' @details
#' Note that unlike `galah`, you cannot set a 'default' provider in `galaxias`;
#' the organisation is always an argument to the function in question. Also 
#' unlike `galah`, `galaxias_config()` enables you to store configuration 
#' details for multiple organisations at once.
#' @name galaxias_config 
#' @param directory A string giving the name of the directory to be used for 
#' storing working files. Defaults to `data-publish`.
#' @param gbif A list containing the entries `username`, `email` and `password`.
#' @export
galaxias_config <- function(directory = NULL,
                            gbif = NULL){
  # check if all arguments are missing
  all_missing <- c(
    is.null(gbif),
    is.null(directory)
  ) |>
    all()
  if(all_missing){ # if so, see whether data are already cached
    if(length(potions::pour(.pkg = "galaxias")) < 1) { # if no caching, set defaults
      galaxias_default_config(directory = "data-publish") |>
        potions::brew(.pkg = "galaxias")
      potions::pour() # for display reasons; i.e. called for `print()`
    }else{ # if something is cached, return it
      potions::pour()
    }
  }else{ # if arguments are supplied, store them
    # first directory
    if(!is.null(directory)){
      if(!inherits(directory, "character")){
        cli::cli_abort("Argument `directory` should be of class `character`")
      }
      potions::brew(directory = directory)
    }
    # then gbif
    if(!is.null(gbif)){
      check_gbif_credentials(gbif)
    } 
    potions::brew(gbif = gbif, method = "leaves")
  }
}

#' Check a list contains only strings
#' @noRd
#' @keywords Internal
check_gbif_credentials <- function(x){
  
  # check is a list
  if(!inherits(x, "list")){
    cli::cli_abort("GBIF credentials should be supplied as a `list`.")
  }
  
  # check all names are supplied, and only those names
  if(!(all(names(x) %in% c("username", "email", "password")) &
       all(c("username", "email", "password") %in% names(x)))){
   cli::cli_abort("GBIF credentials should be named `username`, `email` and `password`.")
  }
  
  # check list only contains characters
  character_check <- purrr::map(x, is.character) |>
    unlist() |>
    all()
  if(!character_check){
    cli::cli_abort("All GBIF credentials should be supplied as strings.")
  }
  
  # check all entries are length 1
  length_check <- all(lengths(x) == 1L)
  if(!length_check){
    cli::cli_abort("All GBIF credentials should be length-1.")
  }
}

#' Set a 'default' object for storing config in `galah`
#' @noRd
#' @keywords Internal
galaxias_default_config <- function(directory){
  x <- list(
    directory = directory,
    gbif = list(username = "",
                email = "",
                password = "")
    # ala = list()
  )
  class(x) <- c("galaxias_config", "list")
  x
}