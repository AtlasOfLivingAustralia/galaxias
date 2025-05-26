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
#' @param quiet (logical) Whether to suppress messages about what is happening. 
#' Default is set to `FALSE`; i.e. messages are shown.
#' @details
#' The `archive` argument must be a full file name including path. The default
#' is `glue::glue("{here::here()}.zip")` which places it adjacent to the working 
#' directory, with the same name, mimicking `devtools::build()`. Sensible
#' alternatives might include:
#' 
#'   - `here::here("my-archive.zip")` for placing _within_ the working directory. 
#'   - `glue::glue("{here::here() |> dirname()}/my-archive.zip")` for placing your file 
#' in the same _directory_ as the default, but with a different file name.
#' @returns An object of class `galaxias_config`, which is a list containing 
#' the cached values. If `galaxias_config()` is used to update (rather than 
#' view) the cache, this is returned invisibly.
#' 
#' @export
galaxias_config <- function(directory = "data-publish",
                            archive = glue::glue("{here::here()}.zip"),
                            gbif = NULL,
                            quiet = FALSE){
  
  # save supplied environment
  x <- as.list(environment())[1:3]
  # see what values are already cached
  cached_config <- potions::pour(.pkg = "galaxias")
  
  # add an exception for onload() - i.e. first time this is called in a session
  if(length(cached_config) < 1) { # if no caching, set defaults
    default_config <- galaxias_default_config(directory = directory,
                                              archive = archive)
      potions::brew(default_config,
                    .pkg = "galaxias")
      invisible(default_config)
  }else{ # i.e. something has been cached
    
    # retrieve cached values and check for updates
    if(!quiet){
      progress_update("Checking configuration")  
    }
    
    # run checks on all objects in environment
    # prevent defaults being overwritten with NULL
    not_null <- purrr::map(x, .f = \(a){!is.null(a)}) |>
      unlist()
    if(all(!not_null)){
      if(!quiet){
        cli::cli_progress_step("All supplied objects are `NULL`; exitting") 
        cli::cli_progress_done()
      }
      return(cached_config)      
    }else{
      x <- x[not_null]
    }
    
    # check remaining arguments
    check_config_args(x)
    
    # overwrite ONLY novel content in cached values
    new_config <- cached_config # ensure all structures are preserved
    update <- FALSE
    for(i in seq_along(x)){
      j <- names(x)[i]
      if(!identical(x[[j]], new_config[[j]])){
        if(!quiet){
          cli::cli_progress_step("Updating {.arg {j}} to {x[[j]]}")
        }
        new_config[[j]] <- x[[j]]
        update <- TRUE
      }
    }
    
    # if new information is given, update cached values
    if(update){
      if(!quiet){
        cli::cli_progress_step("Caching")
        cli::cli_progress_done()
      }
      potions::brew(new_config, method = "modify")
      invisible(new_config)
    }else{ # otherwise view current settings
      if(!quiet){
        cli::cli_progress_step("No changes found")
        cli::cli_progress_done() 
      }
      return(cached_config)
    }
  }
}

#' Internal function to check whether supplied arguments to galaxias_config() are ok
#' @param x a list returned by `environment() |> as.list()`
#' @noRd
#' @keywords Internal
check_config_args <- function(x){
  
  # gbif
  if(!is.null(x$gbif)){
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