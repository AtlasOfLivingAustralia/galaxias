#' Check whether an archive meets the Darwin Core Standard via API
#' 
#' @description
#' Check whether a specified Darwin Core Archive is ready for
#' sharing and publication, according to the Darwin Core Standard. 
#' `check_archive()` tests the specified archive using an online validation 
#' service by sending the archive via an API and returning the results. 
#' Currently only supports validation using GBIF.
#' @returns Invisibly returns a tibble to the workspace containing validation 
#' results; but primarily called for the side-effect of generating a report in 
#' the console.
#' @examples \dontrun{
#' # add GBIF login details
#' galaxias_config(gbif = list(username = "your-gbif-username",
#'                             email = "your-gbif-email",
#'                             password = "your-gbif-password"))
#' 
#' # Check archive against Darwin Core Standard criteria
#' check_archive("dwc-archive.zip")
#' }
#' @details
#' Results returned by `check_archive()` are directly from the institution API, 
#' not from galaxias.
#' @seealso `check_directory()` which runs checks on a folder directory locally, 
#' rather than via API.
#' @order 1
#' @export
check_archive <- function(){
  
  # run checks on `file`
  file <- potions::pour("archive",
                        .pkg = "galaxias")
  check_config_path(file)
  if(!grepl(".zip$", file)){
    bullets <- c(
      "Must supply a zip file.",
      i = "`check_archive()` only accepts a completed Darwin Core Archive saved as a zip file."
    )
  }
  
  # POST query to GBIF validator API
  post_response <- api_gbif_validator_post(source)
  # if there is an error, this function should return `post_response`
  # to allow the user to retry later using the `key` arg, 
  # supplied to `get_report()`
  
  # GET status of query
  # NOTE: This will require a loop with rate-limiting to ensure success
  # see galah-R/R/check_queue.R
  # NOTE: `get_report()` is also an exported function
  status_response <- get_report(post_response$key)
  
  # Q: Should there be an invisible() here to return a tibble?
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
