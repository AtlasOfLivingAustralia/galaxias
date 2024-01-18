#' Check a `dwca` for consistency with the standard
#' 
#' @details
#' `type = "remote"` uses the ALA `validate` API, which in turn calls 
#' `dwca-validator` Python. `type = "local"` duplicates this functionality 
#' via `{pointblank}`
#' @name check
#' @export
check <- function(.dwca, 
                  type = c("local", "remote")){
  type <- match.arg(type)
  switch(type,
         "local" = check_local(.dwca),
         "remote" = check_remote(.dwca))
}

#' @rdname check
#' @param .dwca An object of class `dwca`, created using `dwca()`
#' @param max_n Optional limit to the number of failed tests allowed
#' @importFrom pointblank col_exists
#' @importFrom pointblank create_agent
#' @importFrom pointblank get_agent_report
#' @importFrom rlang abort
#' @importFrom rlang inform
#' @export
check_local <- function(.dwca,
                        max_n = NULL){
  # error catching
  if(!inherits(.dwca, "dwca")){
    abort("`check` only accepts `dwca` objects")
  }
  
  # set up storage object
  report <- vector(mode = "list", length = length(.dwca))
  names(report) <- names(.dwca)
  
  # check occurrences data via {pointblank}
  if(any(names(.dwca) == "occurrences")){ # not sure of terminology or structure here yet
    report$occurrences <- check_occurrences(.dwca$occurrences) 
  }
  
  # metadata checking via `xml_validate()`
  if(is.null(.dwca$metadata)){
    inform("No metadata supplied") # or add this to report?
  }else{
    report$metadata <- check_metadata(.dwca$metadata)
  }
  
  report
}
# NOTE: at present, this only returns a list of tibbles
# Each tibble needs to be parsed to say something informative
# We also need to use these tibbles to report to the console (presumably via `rlang::inform()`)

#' @rdname check
#' @export
check_remote <- function(.dwca = NULL, 
                         file = NULL){
  # checking
  if(is.null(.dwca) & is.null(file)){
    abort("One of `.dwca` or `file` must be supplied")
  }else{
    if(!is.null(.dwca)){
      .dwca <- build(.dwca)
      post_validate(file = .dwca$path)
    }else{
      post_validate(file = file)
    }
  }
}

#' internal function to post content to the `validate` API
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