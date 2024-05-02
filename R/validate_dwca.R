#' Validate a DwCA using the Atlas of Living Australia API
#' 
#' Typically, most testing is done using `devtools::test`; sensible tests for
#' Darwin Core formats can be created using`add_bd_testthat()`. Prior to 
#' publication, however, it is often useful to check suitability against the 
#' host institutions criteria. This function uses the ALA to validate DwCAs.
#' @param path path to a nominated Biodiversity Data Package
#' @param file optionally specify a pre-built DwCA instead of a directory
#' @export
validate_dwca <- function(path = ".", 
                         file = NULL){
  # checking
  if(is.null(path) & is.null(file)){
    abort("One of `path` or `file` must be supplied")
  }else{
    if(!is.null(path)){
      tempfile <- tempdir()
      build_dwca(path, store = tempfile)
      post_validate(file = tempfile)
    }else{
      post_validate(file = file)
    }
  }
}

#' Internal function to post content to the `validate` API
#' @importFrom httr2 request
#' @importFrom httr2 req_body_file
#' @importFrom httr2 req_perform
#' @noRd
#' @keywords Internal
post_validate <- function(file){
  request("https://publish-ws.dev.ala.org.au/validate") |>
    # add JWT token here
    req_body_file(file) |>
    req_perform()
}