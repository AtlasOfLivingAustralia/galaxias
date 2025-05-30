#' Use a metadata statement in a Darwin Core Archive
#' 
#' @description
#' A metadata statement lists the owner of the dataset, how it was collected,
#' and how it can be used (i.e. its' licence). This function reads and
#' converts metadata saved in markdown (.md), Rmarkdown (.Rmd) or Quarto (.qmd) 
#' to xml, and saves it in the publishing directory. This directory is 
#' set using [galaxias_config()] and defaults to `"data-publish"`. folder.
#' 
#' This function is a convenience wrapper function of [delma::read_md()] and 
#' [delma::write_eml()]. 
#' @param file A metadata file in Rmarkdown (`.Rmd`) or Quarto markdown (`.qmd`)
#' format.
#' @param overwrite By default, `use_metadata()` will not 
#'   overwrite existing files. If you really want to do so, set this to `TRUE`. 
#' @param quiet Whether to message about what is happening. Default is set to 
#'  `FALSE`. 
#' @details
#' To be compliant with the Darwin Core Standard, the schema file **must** be
#' called `eml.xml`, and this function enforces that.
#' @returns Does not return an object to the workspace; called for the side
#' effect of building a file in the `data-publish` directory.
#' @seealso [use_metadata_template()] to create a metadata statement template.
#' @examples
#' \dontshow{
#' .old_wd <- setwd(tempdir())
#' }
#' use_metadata_template(quiet = TRUE)
#' use_metadata()
#' \dontshow{
#' setwd(.old_wd)
#' }
#' @export
use_metadata <- function(file = NULL,
                         overwrite = FALSE,
                         quiet = FALSE){
  
  if(is.null(file)){
    cli::cli_abort(c("Missing {.arg file}, with no default.",
                     i = "Must supply path to existing metadata statement file."))
  }
  # `delma::read_md()` runs checks on whether file exists
  
  # import file, ensure EML metadata is added, convert to XML
  if (!quiet) {
    progress_update("Reading metadata statement...")
  }
  metadata_tibble <- delma::read_md(file)
  
  # set up file paths, directories etc.
  directory <- check_publish_directory(quiet = quiet)
  file_path <- fs::path(directory, "eml.xml")
  
  # set writing behaviour
  if(fs::file_exists(file_path)){
    if(overwrite){
      if(!quiet){
        cli::cli_progress_step("Overwriting {.file {file_path}}.")
      }
      
      delma::write_eml(metadata_tibble, file = file_path)
      
      if(!quiet){
        cli::cli_progress_done()
      }
    }else{
      c("{.file {file_path}} already exists.",
        i = "Use `overwrite = TRUE` to overwrite.") |>
        cli::cli_inform()     
    }
  }else{
    if(!quiet){
      cli::cli_progress_step("Writing {.file {file_path}}.")
    }
    
    delma::write_eml(metadata_tibble, file = file_path)
    
    if(!quiet){
      cli::cli_progress_done()
      }
  }
}
