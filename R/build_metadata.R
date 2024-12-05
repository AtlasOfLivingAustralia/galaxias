#' Create a metadata statement for a Darwin Core Archive
#' 
#' A metadata statement lists the owner of the dataset, how it was collected,
#' and how it may used (i.e. its' licence). This function simply converts
#' metadata stored in a markdown file to xml, and stores it in the folder 
#' specified using the `directory` argument.
#' 
#' This function is a fairly shallow wrapper on top of functionality build
#' in the `paperbark` package, particularly `read_md()` and `write_eml()`. You can 
#' use that package to gain greater control, or to debug problems, should you 
#' wish.
#' @param path Path to a metadata statement stored in markdown format (.md).
#' @param file A file where the result should be saved. Defaults to 
#' `data/eml.xml`.
#' @returns Does not return an object to the workspace; called for the side
#' effect of building a file named `meta.xml` in the `data` directory.
#' @importFrom paperbark read_md
#' @importFrom paperbark write_eml
#' @export
build_metadata <- function(x = "data", 
                           file = "./data/eml.xml"){
  if(!file.exists(x)){
    cli::cli_abort("{.file {x}} doesn't exist in specified location.")
  }
  # import file, ensure EML metadata is added, convert to XML
  progress_update("Reading file...")
  metadata_file <- read_md(x)

  progress_update("Writing file...")
  write_eml(built_file, file = file)
  
  cli::cli_alert_success("Metadata successfully built. Saved as {.file /data/eml.xml}.")
  cli::cli_progress_done()
}
