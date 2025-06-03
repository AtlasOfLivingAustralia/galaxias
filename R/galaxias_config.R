#' Provide configuration information to `galaxias`
#' 
#' To validate your dataset, you need to provide credentials to the relevant web 
#' service. This function allows you to store that information for access by 
#' `galaxias` API functions. This function also enables you to change the 
#' `directory` where working documents are stored, and the name of the `archive` 
#' file where the resulting zip file will be placed.
#' @name galaxias_config 
#' @order 1
#' @param directory Path of the directory that will store working files. 
#' Defaults to `data-publish`.
#' @param archive Name of the zip file where the completed archive will be
#' built. Defaults to `dwc-archive.zip`. Note this file will always be
#' saved in the parent directory as per behaviour of `devtools::build()`. The 
#' complete path is automatically generated, and can be viewed in the output of 
#' `galaxias_config()`.
#' @param gbif An (optional) list containing the entries `username`, `email` and 
#' `password`. Only required if you intend to call [check_archive()].
#' @param quiet (logical) Whether to suppress messages about what is happening. 
#' Default is set to `FALSE`; i.e. messages are shown.
#' @returns An object of class `galaxias_config`, which is a list containing 
#' the cached values. If `galaxias_config()` is used to update (rather than 
#' view) the cache, this is returned invisibly.
#' @examples
#' \dontshow{
#'  cached_directory <- galaxias_config()$directory
#' }
#' # show cached entries
#' galaxias_config()
#' 
#' # update an entry
#' galaxias_config(directory = "my-data-lives-here")
#' \dontshow{
#' galaxias_config(directory = cached_directory)
#' }
#' @export
galaxias_config <- function(directory = NULL,
                            archive = NULL,
                            gbif = NULL,
                            quiet = FALSE){
  
  # see what values are already cached
  cached_config <- potions::pour(.pkg = "galaxias")
  
  # add an exception for onload() - i.e. first time this is called in a session
  if(length(cached_config) < 1) { # if no caching, set defaults
    default_config <- galaxias_default_config(directory = directory,
                                              archive = archive)
      potions::brew(default_config,
                    .pkg = "galaxias")
      invisible(default_config)
  }else{ # i.e. something has been cached previously
    
    # check whether all data-related arguments are null
    all_null <- is.null(directory) &
                is.null(archive) &
                is.null(gbif)
    
    # if so, display cached results without updating
    if(all_null){
      return(cached_config)
    }else{ # otherwise, update
      final_config <- cached_config |>
        check_config_directory(directory, quiet = quiet) |>
        check_config_archive(archive, quiet = quiet) |>
        check_config_gbif(gbif, quiet = quiet)
      
      if(!quiet){
        cli::cli_progress_step("Caching")
        cli::cli_progress_done()
      }
      potions::brew(final_config, method = "modify")
      invisible(final_config)
    }
  }
}

#' Set a 'default' object for storing config in `galah`
#' @noRd
#' @keywords Internal
galaxias_default_config <- function(directory,
                                    archive){
  if(is.null(directory)){
    directory <- fs::path("data-publish")
  }
  if(is.null(archive)){
    archive <- fs::path("dwc-archive.zip")
  }
  x <- list(
    directory = directory,
    archive = archive,
    gbif = list(username = "",
                email = "",
                password = ""))
  class(x) <- c("galaxias_config", "list")
  x
}

#' Internal function to update directory arg
#' @param x a list object
#' @noRd
#' @keywords Internal
check_config_directory <- function(x, 
                                   directory, 
                                   quiet,
                                   error_call = rlang::caller_env()){
  if(is.null(directory)){
    x
  }else{
    if(!inherits(directory, "character")){
      wrong_class <- class(directory)
      cli::cli_abort("{.arg directory} should be of class character, not {wrong_class}.",
                     call = error_call)
    }
    
    if(!quiet){
      cli::cli_progress_step("Updating {.arg directory} to {.file {directory}}")
    }
    x$directory <- fs::path(directory)
    x
  }
}

#' Internal function to update archive arg
#' @param x a list object
#' @noRd
#' @keywords Internal
check_config_archive <- function(x, 
                                 archive, 
                                 quiet,
                                 error_call = rlang::caller_env()){
  if(is.null(archive)){
    x
  }else{
    if(!inherits(archive, "character")){
      wrong_class <- class(archive)
      cli::cli_abort("{.arg archive} should be of class character, not {wrong_class}.",
                     call = error_call)
    }
    if(!grepl(".zip$", archive)){
      cli::cli_abort("{.arg archive} must specify a file name ending with `.zip`.",
                     call = error_call)
    }
    
    # convert absolute paths to relative paths
    if(fs::is_absolute_path(archive)) {
      archive <- fs::path_rel(archive, fs::path_wd())
    } else {
      archive <- fs::path(archive)
    }
    
    if(!quiet){
      cli::cli_progress_step("Updating {.arg archive} to {.file {archive}}")
    }
    x$archive <- archive
    x
  }
}

#' Internal function to update GBIF arg
#' @param x a list object
#' @noRd
#' @keywords Internal
check_config_gbif <- function(x, 
                              gbif, 
                              quiet,
                              error_call = rlang::caller_env()){
  if(is.null(gbif)){
    x
  }else{
    # check is a list
    if(!inherits(gbif, "list")){
      cli::cli_abort("GBIF credentials should be supplied as a `list`.",
                     call = error_call)
    }
    
    # check all names are supplied, and only those names
    required_names <- c("username", "email", "password")
    if(!all(names(gbif) %in% required_names) |
       !all(required_names %in% names(gbif))
      ){
      cli::cli_abort("GBIF credentials should be named `username`, `email` and `password`.",
                     call = error_call)
    }
    
    # check list only contains characters
    character_check <- purrr::map(gbif, is.character) |>
      unlist() |>
      all()
    if(!character_check){
      cli::cli_abort("All GBIF credentials should be supplied as strings.",
                     call = error_call)
    }
    
    # check all entries are length 1
    length_check <- all(lengths(gbif) == 1L)
    if(!length_check){
      cli::cli_abort("All GBIF credentials should be length-1.",
                     call = error_call)
    }
    
    if(!quiet){
      cli::cli_progress_step("Updating GBIF credentials")
    }
    x$gbif <- gbif
    x
  }
}
