#' Validate a Darwin Core Archive via API
#' 
#' Validation is a process of checking whether a specified archive is ready for
#' sharing and publication, according to the Darwin Core standard. 
#' @name validate_archive
#' @param x (string) Either a directory containing all the files to be stored in 
#' the archive, or a filename (ending in .zip) of the specified archive. 
#' Defaults to the `data-publish` folder within the current working directory.
#' @param provider (string) The institution to be queried for validation 
#' services. Currently only `"GBIF"` is supported.
#' @returns Invisibly returns a tibble to the workspace containing validation 
#' results; but primarily called for the side-effect of generating a report in 
#' the console.
#' @seealso `check_archive()` which runs checks locally, rather than via API.
#' @order 1
#' @export
validate_archive <- function(x = "data-publish",
                             provider = "GBIF"){

  # if this isn't a zip file, build one, and return the location
  if(!grepl(".zip$", x)){
    x <- build_archive(x)
  }
  
  # POST query to GBIF validator API
  post_response <- api_gbif_validator_post(x)
  # if there is an error, this function should return `post_response`
  # to allow the user to retry later using the `key` arg, 
  # supplied to `get_validator_report()`
  
  # GET status of query
  # NOTE: This will require a loop with rate-limiting to ensure success
  # see galah-R/R/check_queue.R
  # note also that `get_validator_report()` is also an exported function
  status_response <- get_validator_report(post_response$key)
  
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
      i = "Try `galaxias_config(gbif = list(email = \"my_email\"))`") |>
    cli::cli_abort()
  }
  
  # build data components
  validation_request_json <- list(
    sourceId = jsonlite::unbox(version_string),
    notificationEmail = email_string) |>
    jsonlite::toJSON() |>
    curl::form_data(type = "application/json")
  validation_request_file <- curl::form_file(filename, 
                                             type = "application/zip")
  
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
    required_names <- c("key", "created", "modified", "status", "metrics")
    if(all(required_names %in% names(result))){
      cli::cli_inform("GBIF validator API returned an unexpected result.")
    }
    class(result) <- c("gbif_validator_post", "list")
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
      i = "Try `galaxias_config(gbif = list(username = \"my_username\"))`.") |>
    cli::cli_abort()
  }
  if(is.null(password)){
    c("No password supplied for GBIF.", 
      i = "Try `galaxias_config(gbif = list(password = \"my_password\"))`.") |>
    cli::cli_abort()
  }
  glue::glue("{username}:{password}") |>
    as.character()
}