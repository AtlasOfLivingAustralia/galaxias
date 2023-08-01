#' Edit metadata template
#'
#' @param path file path where metadata template will be copied to
#'
#' @return template file opened in RStudio for user to edit
#' @export
#' @importFrom usethis edit_file
#'
#' @examples
#' \dontrun{
#' edit_metadata_md()
#' }
edit_metadata_md <- function(path = ".") {
  # Template stored in inst/markdown/user_template.md
  path_to_md_template <- system.file("markdown/user_template.md", package = "galaxias")

  # Copy the template to root of directory
  file.copy(path_to_md_template, ".")

  # Edit in RStudio
  edit_file(paste0(path, "/user_template.md"))

  # TODO:
  # Need a mechanism to rename the copied file, perhaps a menu() prompt and wraps around file.rename()
  # Checks if its existing, if so DO NOT overwrite
  # AS: See my shiny app for ideas
}
