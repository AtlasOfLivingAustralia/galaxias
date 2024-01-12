#' Get example metadata from ALA
#' 
#' Wrapper function to show some example metadata
#' @param search A string to search using `galah::search_all_collections()`
#' @param id An ALA data resource number
#' @param file File name to save the resulting markdown
#' @importFrom galah search_all
#' @importFrom rlang abort
#' @export
get_metadata_file <- function(search = NULL,
                         id = NULL,
                         file = "metadata.rmd"){
  if(!is.null(id)){
    url <- paste0("https://collections.ala.org.au/ws/dataResource/", id)
  }else if(!is.null(search)){
    result <- search_all("datasets", query = search)
    if(nrow(result) > 0){
      url <- result$uri[1]
    }else{
      abort("No datasets found")
    }
  }
  
  if(is.null(url)){ # if none supplied get template
    path_to_md_template <- system.file(
      "markdown/user_template.md",
      package = "galaxias"
    )
    file.copy(path_to_md_template, file)
  }else{
    write_md(xml = url, 
             file = file,
             title = "Example metadata file")    
  }
  # edit_file(file) # optionally open the file for editting
}