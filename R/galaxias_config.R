#' Provide configuration information to `galaxias`
#' 
#' To validate (or in future, to publish) your dataset, you need to provide
#' credentials to the relevant web service. This function allows you to store
#' that information for access by `galaxias` API functions. This function 
#' also enables you to change the directory where working documents are stored,
#' which defaults to `data-publish`.
#' @name galaxias_config 
#' @order 1
#' @param directory A string giving the name of the directory that will contain
#' working files. Defaults to `data-publish`.
#' @param archive A string giving the name of the archive file, created using
#' [build_archive()] and checked with [check_archive()]. Must include the 
#' full file path; see details for more information.
#' @param gbif An (optional) list containing the entries `username`, `email` and 
#' `password`. Only required if you intend to call [check_archive()].
#' @details
#' The `archive` argument must be a full file name including path. The default
#' is `glue::glue("{here::here()}.zip")` which places it adjacent to the working 
#' directory, with the same name, mimicking `devtools::build()`. Sensible
#' alternatives might include:
#' 
#'   - `here::here("my-archive.zip")` for placing _within_ the working directory. 
#'   - `glue::glue("{here::here() |> dirname()}/my-archive.zip")` for placing your file 
#' in the same _directory_ as the default, but with a different file name.
#' @export
galaxias_config <- function(directory = "data-publish",
                            archive = glue::glue("{here::here()}.zip"),
                            gbif = NULL){
  
  # save supplied environment
  x <- environment() |> as.list()

  # check if all arguments have not been updated (formerly NULL)
  all_default <- directory == "data-publish" &
    archive == glue::glue("{here::here()}.zip") &
    is.null(gbif)

  if(all_default){ # if so, see whether data are already cached
    if(length(potions::pour(.pkg = "galaxias")) < 1) { # if no caching, set defaults
      galaxias_default_config(directory = directory,
                              archive = archive) |>
        potions::brew(.pkg = "galaxias")
      potions::pour() # for display reasons; i.e. called for `print()`
    }else{ # if something is cached, return it
      potions::pour()
    }
  }else{ # if arguments are supplied, store them
    # get cached values
    cached_config <- potions::pour(.pkg = "galaxias")

    # check supplied values
    # GBIF
    x <- x[unlist(purrr::map(x, .f = \(a){!is.null(a)}))] # prevent defaults being overwritten with NULL
    if(!is.null(x$gbif)){
      check_gbif_credentials(x$gbif)
    }
    # directory
    if(!is.null(x$directory)){
      if(!inherits(x$directory, "character")){
        cli::cli_abort("{.arg directory} should be of class `character`.")
      }
    }
    # archive
    if(!is.null(x$archive)){
      if(!inherits(x$archive, "character")){
        cli::cli_abort("{.arg archive} should be of class `character`.")
      }
      if(!grepl(".zip$", x$archive)){
        cli::cli_abort("{.arg archive} must specify a file name ending with `.zip`.")
      }
      if(!file.exists(dirname(x$archive))){
        cli::cli_abort(c("{.arg archive} must specify a valid path.",
                       i = "Specified path: {.file {x$archive}}"))
      }
    }
    
    # overwrite ONLY novel content
    new_config <- cached_config # ensure all structures are preserved
    for(i in seq_along(x)){
      j <- names(x)[i]
      if(!identical(x[[j]], new_config[[j]])){
        new_config[[j]] <- x[[j]]
      }
    }
    
    # cache
    potions::brew(new_config, method = "modify")
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
galaxias_default_config <- function(directory,
                                    archive){
  x <- list(
    directory = directory,
    archive = archive,
    gbif = list(username = "",
                email = "",
                password = "")
    # ala = list()
  )
  class(x) <- c("galaxias_config", "list")
  x
}