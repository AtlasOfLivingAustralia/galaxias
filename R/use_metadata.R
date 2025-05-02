#' Use a metadata statement in a Darwin Core Archive
#' 
#' @description
#' A metadata statement lists the owner of the dataset, how it was collected,
#' and how it can be used (i.e. its' licence). This function reads and
#' converts metadata saved in markdown (.md), Rmarkdown (.Rmd) or Quarto (.qmd) 
#' to xml, and saves it in the `destination` folder.
#' 
#' This function is a convenience wrapper function of [delma::read_md()] and 
#' [delma::write_eml()]. 
#' @param source A metadata file in markdown (`.md`), Rmarkdown (`.Rmd`) or 
#' Quarto markdown (`.qmd`). Defaults
#' to `metadata.md`, which is the same as is created by [use_metadata_template()]
#' @param destination A file name to save the resulting `.xml` file. Defaults to 
#' `eml.xml`. Note that the file is saved to the `data-publish` directory, 
#' unless changed using the `directory` argument of [galaxias_config()].
#' @param overwrite By default, `use_metadata()` will not 
#'   overwrite existing files. If you really want to do so, set this to `TRUE`. 
#' @returns Does not return an object to the workspace; called for the side
#' effect of building a file in the `data-publish` directory.
#' @seealso [use_metadata_statement()] to create a metadata statement template.
#' @export
use_metadata <- function(source = "metadata.Rmd", 
                         destination = "eml.xml",
                         overwrite = FALSE){
  
  # check file existence
  if(!file.exists(source)){
    cli::cli_abort("File {.file {source}} doesn't exist.")
  }
  
  # import file, ensure EML metadata is added, convert to XML
  progress_update("Reading metadata statement...")
  metadata_tibble <- delma::read_md(source)
  
  # set up file paths, directories etc.
  directory <- potions::pour("directory",
                             .pkg = "galaxias")
  usethis::use_directory(directory)
  destination <- fs::path(directory, destination)
  
  # set writing behaviour
  if(file.exists(destination)){
    if(overwrite){
      cli::cli_progress_step("Overwriting {.file {destination}}.")        
      delma::write_eml(metadata_tibble, file = destination)
      cli::cli_progress_done()
    }else{
      c("{.file {destination}} already exists.",
        i = "Set `overwrite = TRUE` to overwrite existing file.") |>
        cli::cli_inform()     
    }
  }else{
    cli::cli_progress_step("Writing {.file {destination}}.")
    delma::write_eml(metadata_tibble, file = destination)
    cli::cli_progress_done()
  }
}
