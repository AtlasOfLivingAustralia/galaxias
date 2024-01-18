#' Get example metadata from ALA
#' 
#' Wrapper function to show some example metadata
#' @name get_metadata_template
#' @order 1
#' @importFrom xml2 read_xml
#' @export
get_metadata_template <- function(){
  system.file(
    "./inst/example_xml/eml_blank.xml",
    package = "galaxias"
  ) |>
    read_xml()
}

#' @rdname get_metadata_template
#' @param search A string to search using `galah::search_all_collections()`
#' @param id An ALA data resource number
#' @order 2
#' @importFrom galah search_all
#' @importFrom rlang abort
#' @importFrom xml2 read_xml
#' @export
get_metadata_example <- function(search = NULL,
                                 id = NULL){
  if(!is.null(id)){
    url <- paste0("https://collections.ala.org.au/ws/dataResource/", id)
  }else if(!is.null(search)){
    result <- search_all("datasets", query = search)
    if(nrow(result) > 0){
      url <- result$uri[1]
    }else{
      abort("No datasets found")
    }
  }else{
    abort("one of `search` or `id` must be supplied")
  }
  read_xml(url)
}