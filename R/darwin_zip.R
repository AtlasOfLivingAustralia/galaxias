#' `darwin_zip` will attempt to zip a complete Darwin Core archive.
#' It will call `darwin_check` to make sure the data conforms to Darwin Core
#' standards. It will also need a valid `meta.xml` file and `eml.xml` file.
#' @param data A data frame of your data
#' @param meta A data frame of your metadata
#' @param eml A data frame of your EML metadata
#' @param path A path to save the zip file to (root by default)
#' @return NULL

darwin_zip <- function(data, meta, eml, path = ".") {
  # Check data
  darwin_check(data, mend = FALSE)

  # Check metadata
}
