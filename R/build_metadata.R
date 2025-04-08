#' Create a metadata statement for a Darwin Core Archive
#' 
#' A metadata statement lists the owner of the dataset, how it was collected,
#' and how it can be used (i.e. its' licence). This function simply reads 
#' converts metadata stored in a markdown file, converts it to xml, and saves it 
#' in the `destination` file.
#' 
#' This function is a fairly shallow wrapper on top of functionality built
#' in the `delma` package, particularly [delma::read_md()] and 
#' [delma::write_eml()]. You can use that package to gain greater control, or to 
#' debug problems, should you wish.
#' @param source A metadata file stored in markdown format (`.md`). Defaults
#' to `metadata.md`, which is the same as is created by [use_metdata()]
#' @param destination A file where the result should be saved. Defaults to 
#' `data-publish/eml.xml`.
#' @returns Does not return an object to the workspace; called for the side
#' effect of building a file named `meta.xml` in the `data-publish` directory.
#' @export
build_metadata <- function(source = "metadata.md", 
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
  cli::cli_alert_success("Metadata successfully built. Saved as {.file destination}.")
  cli::cli_progress_done()
}
