#' Use a metadata statement in a Darwin Core Archive
#' 
#' @description
#' A metadata statement lists the owner of the dataset, how it was collected,
#' and how it can be used (i.e. its licence). This function reads and
#' converts metadata saved in markdown (.md), Rmarkdown (.Rmd) or Quarto (.qmd) 
#' to xml, and saves it in the `destination` folder.
#' 
#' This function is a convenience wrapper function of [delma::read_md()] and 
#' [delma::write_eml()]. 
#' @param source A metadata file in markdown (`.md`), Rmarkdown (`.Rmd`) or 
#' Quarto markdown (`.qmd`). Defaults
#' to `metadata.md`, which is the same as is created by [use_metdata_template()]
#' @param destination A file name to save the resulting `.xml` file. Defaults to 
#' `data-publish/eml.xml`.
#' @returns Does not return an object to the workspace; called for the side
#' effect of building a file named `meta.xml` in the `data-publish` directory.
#' @seealso [use_metadata_statement()] to create a metadata statement template.
#' @export
use_metadata <- function(source = "metadata.md", 
                           destination = "./data-publish/eml.xml"){
  if(!file.exists(source)){
    cli::cli_abort("`{source}` doesn't exist in specified location.")
  }
  # import file, ensure EML metadata is added, convert to XML
  progress_update("Reading metadata statement...")
  metadata_file <- delma::read_md(x)
  
  progress_update("Writing EML file...")
  delma::write_eml(built_file, file = file)
  
  usethis::use_directory("data-publish")
  cli::cli_alert_success("Metadata successfully built. Saved as {.file {destination}}.")
  cli::cli_progress_done()
}
