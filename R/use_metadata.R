#' Write an example metadata statement
#' 
#' This function takes `metadata_example` and exports it to the specified file.
#' This is useful for creating a boilerplate metadata statement that you
#' can edit yourself.
#' @param file (string) A filename to save the statement to. Defaults to 
#' `"metadata.md"`.
#' @param overwrite (logical) Should any existing file with this name be 
#' overwritten? Defaults to `FALSE`.
#' @returns Does not return an object to the workspace; called for the 
#' side-effect of saving a markdown file to the specified location.
#' @importFrom glue glue
#' @importFrom paperbark write_md
#' @importFrom rlang abort
#' @export
use_metadata <- function(file, overwrite = FALSE){
  if(missing(file)){
    file <- "metadata.md"
  }
  if(overwrite){
    write_md(paperbark::metadata_example, file = file)
  }else{
    if(file.exists(file)){
      abort(c(glue("file `{file}` already exists."),
              i = "give a different `file` or set `overwrite = TRUE`"))
    }else{
      write_md(paperbark::metadata_example, file = file)
    }
  }
}