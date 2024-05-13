#' Internal function to build a metadata statement
#' 
#' Uses DESCRIPTION. Called by `use_bd_metadata()`.
#' @importFrom devtools as.package
#' @importFrom usethis use_template
#' @noRd
#' @keywords Internal
build_metadata <- function(){
  desc_list <- as.package(".")
  authors <- desc_list$`authors@r` |>
    parse(text = _) |>
    eval()
  use_template(template = "pkg-metadata",
               save_as = "inst/metadata.md",
               data = list("Title" = desc_list$title,
                           "Description" = desc_list$description,
                           "Licence" = desc_list$licence,
                           "Creator" = get_creator(desc_list)
                           # would be cool to extract CITATION here too
                           ),
               package = "galaxias")
}

#' Internal function to paste Creator from DESCRIPTION
#' 
#' FIXME: This function isn't generating anything rn
#' @noRd
#' @keywords Internal
get_creator <- function(x){
  if(is.null(x$`authors@r`)){
    ""
  }else{
    authors <- x$`authors@r` |>
      parse(text = _) |>
      eval()   
    result <- lapply(authors, function(a){
      if("cre" %in% a$role){
        paste(a$given, a$family, sep = " ")
      }else{
        NULL
      }
    })
    unlist(result)
  }
}