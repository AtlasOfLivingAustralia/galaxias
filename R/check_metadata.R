#' Check that a supplied metadata statement is valid according to GBIF schema
#' @param metadata xml object representing metadata
#' @return tibble showing parsed errors
#' @noRd
#' @keywords Internal
check_metadata <- function(metadata) {
  
  # 1. validate against "./data-raw/eml-gbif-profile.xsd" with xml2::xml_validate()
  validator <- xml_validate(
    metadata, 
    eml_validator_xsd)
  
  if(!validator){
    errors_df <- attr(validator, "errors") |>
                 parse_validator_errors()
  }else{
    errors_df <- tibble()
  }
  
  # 2. check for mandatory fields? Or is this handled above?
  # bind_rows(errors_df, something)
  
  errors_df
}

#' Vector of field terms required by ALA
#' Q: should we be checking requirements that only ALA has?
#' Q: are these checked for by `xml_validate()`?
#' @noRd
#' @keywords Internal
required_fields <- function(){
  c(
  "title",
  "abstract",
  # not sure if referencePublication is equivalent to "citation" in ALA terms
  "referencePublication",
  "creator",
  "intellectualRights"
)}


#' Internal function to extract information from `xml_validate()` error strings
#' @noRd
#' @keywords Internal
parse_validator_errors <- function(strings){
  strings <- strings[!grepl("Skipping import of schema", x = strings)]
  element <- str_extract(strings, "^Element '[[:graph:]]+'") |>
    gsub("^Element '|'$", "", x = _)
  
  elements_list <- str_extract(strings, "':([[:graph:]]|\\s)+") |>
    sub("':\\s", "", x = _) |>
    strsplit("\\.\\s") 
  lapply(elements_list, \(x){
    if(length(x) < 2){
      x[[2]] <- ""
    }
    names(x) <- c("description", "remedy")
    x
  }) |>
    bind_rows() |>
    mutate(element = element, .before = "description")
}