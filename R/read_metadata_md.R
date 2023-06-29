#' Read metadata md information into R
#'
#' @param path file path pointing .md metadata template
#'
#' @return tibble containing concatenated strings from each field
#' @keywords internal
#' @examples
#' path_to_md_template <- system.file("markdown/westerband_template.md", package = "galaxia")
#' metadata_ls <- read_metadata_md(path_to_md_template)

read_metadata_md <- function(path){
  scan_output <- scan(path,
                      what = "character", 
                      sep = "\n", 
                      blank.lines.skip = FALSE)
  
  return(scan_output)
}