#' Check whether an archive meets the Darwin Core Standard via API
#' 
#' @description
#' Check whether a specified Darwin Core Archive is ready for
#' sharing and publication, according to the Darwin Core Standard. 
#' [check_archive()] tests the specified archive using an online validation 
#' service by sending the archive via an API and returning the results. 
#' Currently only supports validation using GBIF.
#' @param wait (logical) Whether to wait for a completed report from the API
#' before exiting (`TRUE`, the default), or try the API once and return the
#' result regardless (`FALSE`).
#' @param quiet (logical) Whether to suppress messages about what is happening. 
#' Default is set to `FALSE`; i.e. messages are shown.
#' @details
#' Note that there is no `file` argument to `check_archive()`; instead, the
#' location where files will be built and checked is set using the
#' `archive` argument to [galaxias_config].
#' 
#' Internally, [check_archive()] both `POST`s the specified archive to the GBIF 
#' validator API and then calls [get_report()] to retrieve (`GET`) the result. 
#' [get_report()] is exported to allow the user to download results at a later 
#' time should they wish; this is more efficient than repeatedly generating 
#' queries with [check_archive()] if the underlying data are unchanged. A third 
#' option is simply to assign the outcome of [check_archive()] or [get_report()] 
#' to an object, then call [view_report()] to format the result nicely. This 
#' approach doesn't require any further API calls and is considerably faster.
#' 
#' Note that information returned by these functions is provided verbatim from 
#' the institution API, not from galaxias.
#' @returns Both [check_archive()] and [get_report()] return an object of class
#' `gbif_validator` to the workspace. [view_report()] and 
#' [print.gbif_validator()] don't return anything, and are called for the
#' side-effect of printing useful information to the console.
#' @examples \dontrun{
#' # add GBIF login details
#' galaxias_config(gbif = list(username = "your-gbif-username",
#'                             email = "your-gbif-email",
#'                             password = "your-gbif-password"))
#' 
#' # Check archive against Darwin Core Standard criteria
#' check_archive("dwc-archive.zip")
#' }
#' @seealso [check_directory()] which runs checks on a folder directory locally, 
#' rather than via API.
#' @order 1
#' @export
check_archive <- function(wait = TRUE,
                          quiet = FALSE){
  

  # run checks on `archive`
  file_name <- potions::pour("archive",
                             .pkg = "galaxias")
  archive <- fs::path_abs(glue::glue("../{file_name}"))
  if(!dir.exists(archive)){
    cli::cli_abort(c("Specified archive {.file {file_name}} does not exist.",
                     x = "Can't find {.path {archive}}."))
  
  # POST query to GBIF validator API
  post_response <- api_gbif_validator_post(archive)
  if(!quiet){
    print_archive_POST(post_response)
  }
  
  if(wait){
    # GET status of query
    # NOTE: `get_report()` is also an exported function and should behave sensibly
    get_report(post_response$key,
               wait = wait,
               quiet = quiet)    
  }else{
    post_response
  }
}


#' Internal function to post content to the GBIF `validator` API
#' https://techdocs.gbif.org/en/openapi/v1/validator#/validation-resource/submitFile
#' @noRd
#' @keywords Internal
api_gbif_validator_post <- function(filename){
  
  # extract strings 
  version_string <- galaxias_version_string()
  userpwd_string <- gbif_username_string()
  email_string <- potions::pour("gbif", "email", .pkg = "galaxias")
  if(is.null(email_string)){
    c("No email supplied for GBIF.", 
      i = "Use `galaxias_config(gbif = list(email = \"my_email\"))`") |>
    cli::cli_abort()
  }
  
  # build data components
  validation_request_json <- list(
    sourceId = jsonlite::unbox(version_string),
    notificationEmail = email_string) |>
    jsonlite::toJSON() |>
    form_json()
  validation_request_file <- form_zip(file_path = filename)

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

#' get supplied username and password
#' @noRd
#' @keywords Internal
gbif_username_string <- function(){
  username <- potions::pour("gbif", "username", .pkg = "galaxias")
  password <- potions::pour("gbif", "password", .pkg = "galaxias")
  if(is.null(username) & is.null(password)){
    cli::cli_abort(c("No username or password supplied for GBIF.", 
            i = "See `?galaxias_config` for help"))
  }
  if(is.null(username)){
    c("No username supplied for GBIF.", 
      i = "Use `galaxias_config(gbif = list(username = \"my_username\"))`.") |>
    cli::cli_abort()
  }
  if(is.null(password)){
    c("No password supplied for GBIF.", 
      i = "Use `galaxias_config(gbif = list(password = \"my_password\"))`.") |>
    cli::cli_abort()
  }
  glue::glue("{username}:{password}") |>
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
