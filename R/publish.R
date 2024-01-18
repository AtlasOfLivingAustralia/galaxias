#' Publish a DwCA to the ALA
#' 
#' Submitted via API
publish <- function(.dwca = NULL, 
                    file = NULL){
  # checking
  if(is.null(.dwca) & is.null(file)){
    abort("One of `.dwca` or `file` must be supplied")
  }else{
    if(!is.null(.dwca)){
      .dwca <- build(.dwca)
      post_publish(file = .dwca$path)
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