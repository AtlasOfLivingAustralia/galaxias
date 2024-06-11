#' Validate a Darwin Core Archive via API
#' 
#' @param path path to a nominated Biodiversity Data Package
#' @param file optionally specify a pre-built DwCA instead of a directory
#' @param provider (string) the institution to be queried for validation 
#' services. Currently only `"GBIF"` is supported.
#' @returns A report on the supplied archive.
#' @export
validate_dwca <- function(pkg = ".", 
                          file = NULL,
                          provider = "GBIF"
                          ){
  galaxias_API(pkg = pkg, 
               file = file, 
               api = "validate",
               provider = provider)
}

#' Internal function to construct a valid DwCA for shipping with API
#' @noRd
#' @keywords Internal
galaxias_API <- function(pkg = ".", 
                         file = NULL, 
                         api){
  # checking
  if(is.null(pkg) & is.null(file)){
    abort("One of `pkg` or `file` must be supplied")
  }
  if(!is.null(pkg)){
    tempfile <- tempdir()
    build_dwca(pkg, path = tempfile)
    file <- glue("{tempfile}/{pkg}.zip")
  }
  post_API(file = file, api = api)
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
                     provider){
  api_string <- glue("{provider}-{api}")
  api <- switch(match.arg(api),
                "GBIF-validate" = "https://api.gbif.org/validation",
                "ALA-validate" = "https://publish-ws.dev.ala.org.au/validate",
                "ALA-publish" = "https://publish-ws.dev.ala.org.au/publish")
  request(api) |>
    # add JWT token here
    req_body_file(file) |>
    req_perform()
}