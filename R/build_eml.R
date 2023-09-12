#' Functions to open metadata markdown templates, and read markdown
#' into R for use.
#' Required metadata fields
# title
# description
# publishing organization
# type
# license
# contact(s)
# creator(s)
# metadata provider(s)
# Recommended metadata fields
# sampling methodology - in situations where data comes from a sampling event
# citation - to ensure your dataset gets cited the way you want

#' @param path file path where metadata template will be copied to
#' @param type type of template (we can support more)
#' @return template file opened in RStudio for user to edit
#' @export
#' @importFrom usethis edit_file
template_edit <- function(path = ".", type = "ALA") {
  # Template stored in inst/markdown/user_template.md
  # Switch if ALA then
  path_to_md_template <- switch(type,
    "ALA" = {
      path_to_md_template <- system.file(
        "markdown/user_template.md",
        package = "galaxias"
      )
    }
  )

  # Copy the template to root of directory
  file.copy(path_to_md_template, ".")

  # Edit in RStudio
  edit_file(paste0(path, "/user_template.md"))
}

#' @export
function_thing <- function(template, output = NULL) {
  rmarkdown::pandoc_convert(normalizePath(template),
    to = "docbook",
    output = paste0(tempdir(), "/test.xml"),
    options = "-s"
  )
  docbook <- xml2::read_xml(paste0(tempdir(), "/test.xml"))
  docbook <- xml2::xml_ns_strip(docbook)
  sections <- xml_find_all(docbook, "//section", flatten = FALSE)
  result_list <- lapply(sections, function(sec) {
    title <- xml_text(xml_find_first(sec, "./title"))
    para <- xml_text(xml_find_all(sec, "./para"))
    list(title = title, para = para)
  })
  xml_ids <- sapply(sections, function(sec) xml_attr(sec, "id"))
  names(result_list) <- xml_ids

  my_eml <- EML::eml$eml(
    dataset = EML::eml$dataset(
      result_list$fish$title,
      methods = result_list$methods$para,
      creator = result_list$creator$para,
      contact = result_list$creator$para
    )
  )
  message("Writing EML to file...\n")
  write_eml(my_eml, "./eml_test.xml")
}

# Define constants for licence you can pick 1, 2, 3
