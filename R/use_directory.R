#' Strictly internal mirror of `usethis::use_directory()` built using 
#' `dir.create()`, to enforce changes relative to working directory, NOT
#' project directory. Important for ensuring tests can build and detect 
#' resulting directories
#' @noRd
#' @keywords Internal
use_directory <- function(path){
  if(path != "."){
    if(!file.exists(path)){
      dir.create(path)
    }    
  }
}