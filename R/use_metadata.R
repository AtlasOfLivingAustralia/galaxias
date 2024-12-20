#' Create an example metadata statement file
#' 
#' @description
#' This function creates a metadata template and exports it to the specified 
#' file. The template is taken from `metadata_example` (see `metadata_example` 
#' for documentation). The template `.md` file can act as a boilerplate metadata 
#' statement that you can edit yourself. 
#' 
#' The final edited file can be converted for use in a Darwin 
#' Core Archive using [build_metadata()]
#' 
#' @param file (string) A filename to save the statement to. Defaults to 
#' `"metadata.md"`.
#' @param overwrite (logical) Should any existing file with this name be 
#' overwritten? Defaults to `FALSE`.
#' 
#' @returns Does not return an object to the workspace; called for the 
#' side-effect of saving a markdown file to the specified location.
#' 
#' @examples \dontrun{
#' # Save template file metadata.md to local directory
#' use_metadata()
#' }
#' 
#' @importFrom glue glue
#' @importFrom paperbark write_md
#' @importFrom rlang abort
#' @importFrom cli cli_progress_step
#' @importFrom cli cli_progress_done
#' @importFrom cli cli_bullets
#' @importFrom cli cli_fmt
#' @importFrom cli col_grey
#' @export
use_metadata <- function(file, overwrite = FALSE){
  if(missing(file)){
    cli_progress_step("Creating template file {.file metadata.md}.")
    file <- "metadata.md"
    cli::cli_progress_done()
  }
  if(overwrite){
    cli_progress_step("Overwriting existing file {.file {file}}.")
    write_md(paperbark::metadata_example, file = file)
    cli_progress_done()
  }else{
    if(file.exists(file)){
      bullets <- c("File {.file {file}} already exists.",
                   i = "Use a different filename or set `overwrite = TRUE`") |>
        cli_bullets() |>
        cli_fmt()
      
      abort(bullets)
    }else{
      write_md(paperbark::metadata_example, file = file)
    }
  }

  cli_bullets(c(
    v = "File template {.file {file}} saved to top folder in local directory.",
    i = paste(
        c(" Edit {.file {file}}") |> col_grey(), 
        c("then use {.fn build_metadata} to build final metadata statement.") |> col_grey()
        )
  ))
  
}