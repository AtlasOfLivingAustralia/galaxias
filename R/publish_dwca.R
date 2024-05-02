#' Publish a DwCA to the ALA
#' 
#' Submitted via API
publish_dwca <- function(path = NULL, 
                    file = NULL){
  # checking
  if(is.null(path) & is.null(file)){
    abort("One of `path` or `file` must be supplied")
  }else{
    if(!is.null(path)){
      # create temporary directory
      build_dwca(path, out = tempdir)
      post_publish(file = tempdir)
    }else{
      post_publish(file = file)
    }
  }
}

#' internal function to post content to the `publish` API
#' @importFrom httr2 request
#' @importFrom httr2 req_body_file
#' @importFrom httr2 req_perform
#' @noRd
#' @keywords Internal
post_publish <- function(file){
  request("https://publish-ws.dev.ala.org.au/publish") |>
    # add JWT token here
    req_body_file(file) |>
    req_perform()
}