#' Check whether an archive meets the Darwin Core Standard via API
#' 
#' @description
#' Check whether a specified Darwin Core Archive is ready for
#' sharing and publication, according to the Darwin Core Standard. 
#' `check_archive()` tests an archive - defaulting to `"dwc-archive.zip"` in
#' the users' parent directory - using an online validation service. Currently 
#' only supports validation using GBIF.
#' @param file The name of the file in the parent directory to pass to the 
#' validator API, ideally created using [build_archive()].
#' @param username Your GBIF username.
#' @param email The email address used to register with `gbif.org`.
#' @param password Your GBIF password.
#' @param wait (logical) Whether to wait for a completed report from the API
#' before exiting (`TRUE`, the default), or try the API once and return the
#' result regardless (`FALSE`).
#' @param quiet (logical) Whether to suppress messages about what is happening. 
#' Default is set to `FALSE`; i.e. messages are shown.
#' @details
#' Internally, `check_archive()` both `POST`s the specified archive to the GBIF 
#' validator API and then calls `get_report()` to retrieve (`GET`) the result. 
#' [get_report()] is exported to allow the user to download results at a later 
#' time should they wish; this is more efficient than repeatedly generating 
#' queries with `check_archive()` if the underlying data are unchanged. A third 
#' option is simply to assign the outcome of `check_archive()` or `get_report()` 
#' to an object, then call `view_report()` to format the result nicely. This 
#' approach doesn't require any further API calls and is considerably faster.
#' 
#' Note that information returned by these functions is provided verbatim from 
#' the institution API, not from galaxias.
#' @returns Both `check_archive()` and `get_report()` return an object of class
#' `gbif_validator` to the workspace. `view_report()` and 
#' `print.gbif_validator()` don't return anything, and are called for the
#' side-effect of printing useful information to the console.
#' @seealso [check_directory()] which runs checks on a directory (but **not**
#' an archive) locally, rather than via API.
#' @order 1
#' @export
check_archive <- function(file = "dwc-archive.zip",
                          username = NULL,
                          email = NULL,
                          password = NULL,
                          wait = TRUE,
                          quiet = FALSE){

  # check if `file` exists
  check_filename(file)
  
  # if so, create path
  archive <- fs::path_abs(glue::glue("../{file}"))
  if(!fs::file_exists(archive)){
    cli::cli_abort(c("Specified archive {.file {file}} does not exist.",
                     x = "Can't find {.path {archive}}."))
  }
  
  # check GBIF credentials
  check_gbif_credentials(username, 
                         email, 
                         password)
  
  # POST query to GBIF validator API
  post_response <- api_gbif_validator_post(archive,
                                           username = username,
                                           email = email,
                                           password = password)
  if(!quiet){
    print_archive_POST(post_response)
  }
  
  # GET status of query
  # NOTE: `get_report()` is also an exported function 
  # and as such should behave sensibly.
  get_report(post_response$key,
             username = username,
             password = password,
             wait = wait,
             quiet = quiet) 
}


#' Internal function to check GBIF credentials
#' @param x a list object
#' @noRd
#' @keywords Internal
check_gbif_credentials <- function(username, 
                                   email, 
                                   password,
                                   email_needed = TRUE,
                                   error_call = rlang::caller_env()){
  
  if(email_needed){ # i.e. check_archive()
    gbif <- list(username = username,
                 email = email, 
                 password = password)
  }else{ # i.e. get_report()
    gbif <- list(username = username,
                 password = password)
  }
  gbif_names <- names(gbif) 
  
  # check all names are supplied, and only those names
  null_check <- purrr::map(gbif, is.null) |>
    unlist()
  if(any(null_check)){
    cli::cli_abort(c("All GBIF credentials should be supplied.",
                    i = "Argument {.arg {gbif_names[which(null_check)[1]]}} is missing", 
                    i = "Visit {.url https://www.gbif.org/user/profile} to register"),
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
}


#' Internal function to post content to the GBIF `validator` API
#' https://techdocs.gbif.org/en/openapi/v1/validator#/validation-resource/submitFile
#' @noRd
#' @keywords Internal
api_gbif_validator_post <- function(file,
                                    username,
                                    email,
                                    password){
  
  # extract strings 
  version_string <- galaxias_version_string()
  userpwd_string <-   glue::glue("{username}:{password}") |>
    as.character()
  email_string <- email
  
  # build data components
  validation_request_json <- list(
    sourceId = jsonlite::unbox(version_string),
    notificationEmail = email_string) |>
    jsonlite::toJSON() |>
    form_json()
  validation_request_file <- form_zip(file_path = file)

  # build query
  query <- httr2::request("https://api.gbif.org/v1/validation") |>
    httr2::req_headers(
      `User-Agent` = version_string,
      `X-USER-AGENT` = version_string,
      `Content-Type` = "multipart/form-data",
       Accept = "application/json") |>
    httr2::req_options(
      httpauth = 1,
      userpwd = userpwd_string) |>
    httr2::req_body_multipart(
      file = validation_request_file,
      validationRequest = validation_request_json)

  # perform query
  result <- httr2::req_perform(query) |>
    httr2::resp_body_json()
  
  # return useful messages and objects to the user
  if(is.list(result)){
    class(result) <- c("gbif_validator", "list")
    result
  }else{
    cli::cli_abort("GBIF validator API did not return a result.")
  }
}


#' get version for galaxias to put in API call headers
#' @noRd
#' @keywords Internal
galaxias_version_string <- function() {
  version_string <- "version-unknown"
  suppressWarnings(
    try(version_string <- utils::packageDescription("galaxias")[["Version"]],
        silent = TRUE))
  glue::glue("galaxias-R-{version_string}") |>
    as.character()
}


#' Internal function to create a JSON form
#' modified from `curl::form_data()`
#' @noRd
#' @keywords Internal
form_json <- function (value) 
{
  if(is.character(value)){
    value <- enc2utf8(value) |>
      glue::glue_collapse(sep = "\n") |>
      as.character() |>
      charToRaw()    
  }
  if(!is.raw(value)){
    cli::cli_abort("Argument 'value' must be string or raw vector")
  }
  list(value = value, 
       type = "application/json") |>
    structure(class = "form_data")
}


#' Internal function to create a file addon
#' modified from `curl::form_file()`
#' @noRd
#' @keywords Internal
form_zip <- function(file_path){
  x <- normalizePath(file_path[1], 
                     mustWork = TRUE) |>
    enc2native()

  list(path = x, 
       type = "application/zip",
       name = NULL) |>
    structure(class = "form_file")
}