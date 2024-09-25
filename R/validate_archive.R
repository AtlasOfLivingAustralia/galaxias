#' Validate a Darwin Core Archive via API
#' 
#' Note: Unclear whether this is available yet, from GBIF or ALA.
#' @param path Path to a nominated archive.
#' @param file Optionally specify a pre-built DwCA instead of a directory.
#' @param provider (string) The institution to be queried for validation 
#' services. Currently only `"GBIF"` is supported.
#' @returns A report on the supplied archive.
#' @noRd
#' @keywords Internal
validate_archive <- function(pkg = ".", 
                             file = NULL, 
                             provider = "GBIF"){
  # checking
  if(is.null(pkg) & is.null(file)){
    abort("One of `pkg` or `file` must be supplied")
  }
  
  if(is.null(file) & !is.null(pkg)){
    tempfile <- tempdir()
    build_archive(pkg, path = tempfile)
    file <- glue("{tempfile}/{pkg}.zip")
  }
  post_API(file = file, 
           api = "validate",
           provider = "GBIF")
}

#' Internal function to post content to the specified `validate` API
#' @importFrom glue glue
#' @importFrom httr2 request
#' @importFrom httr2 req_body_file
#' @importFrom httr2 req_perform
#' @noRd
#' @keywords Internal
post_API <- function(file, 
                     api = c("validate", "publish"),
                     provider = "GBIF"){
  api <- match.arg(api)
  api_string <- glue("{provider}-{api}")
  api_url <- switch(api_string,
                    "GBIF-validate" = "https://api.gbif.org/validation",
                    "ALA-validate" = "https://publish-ws.dev.ala.org.au/validate",
                    "ALA-publish" = "https://publish-ws.dev.ala.org.au/publish")
  # username <- pour("gbif", "username", .pkg = "galaxias")
  # password <- pour("gbif", "password", .pkg = "galaxias")
  # userpwd_string <- glue("{username}:{password}") |>
  #   as.character()
  result <- request(api_url) |>
    req_headers(`User-Agent` = "galaxias-v0.1.0.9999",
                # `X-USER-AGENT` = "galaxias-v0.1.0.9999",
                `Content-Type` = "multipart/form-data",
                Accept = "application/json") |>
    # req_options(httpauth = 1,
    #             userpwd = userpwd_string) |>
    req_body_file(path = file)
  
  
    req_perform()
}