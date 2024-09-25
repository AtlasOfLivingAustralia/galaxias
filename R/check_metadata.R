#' Check that a supplied metadata statement is valid according to GBIF schema
#' 
#' Note: this function requires the user to be online.
#' @param metadata xml object representing metadata
#' @return A tibble showing parsed errors
#' @importFrom xml2 xml_validate
#' @importFrom xml2 read_xml
#' @export
check_metadata <- function(metadata) {
  
  # 1. validate against "./data-raw/eml-gbif-profile.xsd" with xml2::xml_validate()
  validator <- xml_validate(
    metadata, 
    read_xml("http://rs.gbif.org/schema/eml-gbif-profile/1.1/eml-gbif-profile.xsd")) 
  # Q: how to store this object internally? seems to fail for some reason
  
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

#' Internal function to extract information from `xml_validate()` error strings
#' @importFrom dplyr bind_rows
#' @importFrom dplyr mutate
#' @importFrom purrr map
#' @importFrom stringr str_extract
#' @noRd
#' @keywords Internal
parse_validator_errors <- function(strings){
  strings <- strings[!grepl("Skipping import of schema", x = strings)]
  element <- str_extract(strings, "^Element '[[:graph:]]+'") |>
    gsub("^Element '|'$", "", x = _)
  elements_list <- str_extract(strings, "':([[:graph:]]|\\s)+") |>
    sub("':\\s", "", x = _) |>
    strsplit("\\.\\s") 
  map(.x = elements_list,
      .f = \(x){
        if(length(x) < 2){
          x[[2]] <- ""
        }
        names(x) <- c("description", "remedy")
        x
      }) |>
    bind_rows() |>
    mutate(element = element, .before = "description")
}