#' Open a markdown template file for creating EML metadata
#' @param path string Directory where metadata template will be copied to,
#'   defaults to current working directory.
#' @param type
#' string Which template flavour to use:
#' * ALA (default)
#' * More to come...
#' @param licence
#' numeric Which licence to use for the dataset:
#' \itemize{
#'   \item 1 = Creative Commons Zero - CC0
#'   \item 2 = Attribution – CC BY (4.0)
#'   \item 3 = Attribution-Noncommercial – CC BY-NC
#' }
#' See
#' [article](https://support.ala.org.au/support/solutions/articles/
#' 6000197133-what-licence-should-i-use-) for more information.
#' @return The template file is opened in RStudio.
#' @export
#' @importFrom usethis edit_file
edit_metadata <- function(path = ".",
                          type = "ALA",
                          licence = 1) {
  path_to_md_template <- switch(type,
    "ALA" = {
      path_to_md_template <- system.file(
        "markdown/user_template.md",
        package = "galaxias"
      )
    }
  )
  file.copy(path_to_md_template, path)
  edit_file(paste0(path, "/user_template.md"))
}