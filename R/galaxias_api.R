#' Validate or Publish a DwCA using the Atlas of Living Australia API
#' 
#' Typically, most testing is done using `devtools::test`; sensible tests for
#' Darwin Core formats can be created using`add_bd_testthat()`. Prior to 
#' publication, however, it is often useful to check suitability against the 
#' host institutions criteria. This function uses the ALA to validate DwCAs.
#' 
#' Once validation has been passed, use `publish_dwca()`.
#' @rdname api_calls
#' @param path path to a nominated Biodiversity Data Package
#' @param file optionally specify a pre-built DwCA instead of a directory
#' @importFrom glue glue
#' @export
validate_dwca <- function(pkg = ".", 
                          file = NULL){
  galaxias_API(pkg = pkg, 
               file = file, 
               api = "validate")
}

#' @rdname api_calls
#' @export
publish_dwca <- function(pkg = ".", 
                         file = NULL){
  galaxias_API(pkg = pkg, 
               file = file, 
               api = "publish")
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
#' @importFrom httr2 request
#' @importFrom httr2 req_body_file
#' @importFrom httr2 req_perform
#' @noRd
#' @keywords Internal
post_API <- function(file, 
                     api = c("validate", "publish")){
  api <- switch(match.arg(api),
                "validate" = "https://publish-ws.dev.ala.org.au/validate",
                "publish" = "https://publish-ws.dev.ala.org.au/publish")
  request(api) |>
    # add JWT token here
    req_body_file(file) |>
    req_perform()
}