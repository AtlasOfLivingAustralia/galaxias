#' Check validity of a metadata statement
#' 
#' In the Darwin Core standard, metadata statements are mandatory and must be
#' provided in Ecological Meta Language (EML) in a file called `eml.xml`. This
#' function applies a series of checks designed by GBIF to check the structure
#' of the specified `filename` for consistency with the standard. Note, 
#' however, that this function doesn't check the _content_ of those files,
#' meaning a file could be structurally sound and still be lacking critical 
#' information.
#' @param filename A local file containing metadata in xml format. This will
#' typically be called `eml.xml` and can be constructed from markdown using
#' `build_metadata()`.
#' @details
#' This function uses local versions of `dc.xsd`, `eml-gbif-profile.xsd` and 
#' `eml.xsd` downloaded from
#'  \link{http://rs.gbif.org/schema/eml-gbif-profile/1.3/} on 2024-09-25.
#' @return Invisibly returns a tibble showing parsed errors; or an empty 
#' tibble if no errors are identified.
#' @importFrom xml2 xml_validate
#' @importFrom xml2 read_xml
#' @export
check_metadata <- function(filename) {
  # get required files
  local_file <- read_xml(filename)
  # NOTE: we use `extdata` here because storing these files in `sysdata.rda`
  # breaks the connections between files, causing validation to fail.
  validator_file <- system.file("extdata", 
                                "eml-gbif-profile.xsd", 
                                package = "galaxias",
                                mustWork = TRUE) |>
    read_xml()
  
  # run validation
  # NOTE: this returns TRUE for valid, and FALSE for invalid
  validator_response <- xml_validate(local_file, 
                                     schema = validator_file)
  
  # process validation errors
  # NOTE: this uses functions defined in `check_occurrences()`
  if(!validator){
    errors_df <- attr(validator_response, "errors") |>
                 parse_validator_errors()
    split(errors_df, seq_len(nrow(errors_df))) |>
      map(~ format_messages_from_checks(.x)) |>
      invisible()
  }else{
    errors_df <- tibble(term = character(),
                        messages = character(),
                        remedy = character())
    
  }

  invisible(errors_df)
}

#' Internal function to extract information from `xml_validate()` error strings
#' @importFrom dplyr bind_rows
#' @importFrom dplyr mutate
#' @importFrom purrr map
#' @importFrom stringr str_extract
#' @noRd
#' @keywords Internal
parse_validator_errors <- function(strings){
  # strings <- strings[!grepl("Skipping import of schema", x = strings)]
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
        names(x) <- c("messages", "remedy")
        x
      }) |>
    bind_rows() |>
    mutate(term = element, .before = "messages")
}