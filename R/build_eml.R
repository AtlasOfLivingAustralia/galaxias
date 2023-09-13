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

# template <- "./inst/markdown/westerband_template_modified.md"
#' @export
function_thing <- function(template, output_dir = NULL) {
  # docbook xml only needed temporarily
  # doc_output <- paste0(tempdir(), "/test.xml")

  rmarkdown::pandoc_convert(normalizePath(template),
    to = "docbook",
    output = paste0(tempdir(), "/temp.xml"),
    options = "-s"
  )
  docbook <- xml2::read_xml(paste0(tempdir(), "/temp.xml"))
  docbook <- xml2::xml_ns_strip(docbook)
  sections <- xml2::xml_find_all(docbook, "//section", flatten = FALSE)
  result_list <- lapply(sections, function(sec) {
    title <- xml2::xml_text(xml2::xml_find_first(sec, "./title"))
    para <- xml2::xml_text(xml2::xml_find_all(sec, "./para"))
    list(title = title, para = para)
  })
  xml_ids <- sapply(sections, function(sec) xml2::xml_attr(sec, "id"))
  names(result_list) <- xml_ids

  # This is where the EML is built, by extracting the relevant information from
  # the result_list, and assinging to the appropriate EML fields
  # TODO: This will be factored out, because it could potentially get quite
  # large? Unsure.

  # I'm going to try write them in the order that they appear in the template
  # my_eml <- EML::eml$eml(
  #   dataset = EML::eml$dataset(
  #     result_list$fish$title,
  #     methods = result_list$methods$para,
  #     creator = result_list$creator$para,
  #     contact = result_list$creator$para
  #   )
  # )

  # Note - the fields are accessed in the result list, first by their markdown
  # tag
  # {#tag}, then by the paragraph element typically
  # methods and projects fields look extensible
  my_eml <- EML::eml$eml(
    packageId = uuid::UUIDgenerate(),
    additionalMetadata = NULL,
    dataset = EML::eml$dataset(
      title = result_list$title$para, # Title
      # ?? == GUID
      # Are these equivalent? Output will depend on the flavour of EML
      abstract = result_list$`public-short-description`$para,
      # ?? == public-description
      # ?? == technical-description
      # ?? == data-quality-description
      # methods == methods-description

      methods = list(
        sampling = result_list$methods$para,
        description = result_list$`data-quality-description`$para,
        random = "hello"
      ),
      purpose = result_list$purpose$para,
      # data-generalisations
      # information-withheld
      # This is different to the reference of the actual dataset
      referencePublication = result_list$citation$para,
      intellectualRights = result_list$license$para,
      licensed = result_list$rights$para,
      creator = result_list$creator$para,
      contact = result_list$creator$para
      # electronicMailAddress
      # individualName
      # project field could be used?
    )
  )

  # ?? == institutions
  # (Primary contacts)
  # ?? == primary-contact-name
  # ?? == primary-contact-email

  # additionalMetadata

  message("Writing EML to file...\n")
  # TODO: I want to be able to specify the output directory and file name
  EML::write_eml(my_eml, "./eml_test.xml")
}

# Define constants for licence you can pick 1, 2, 3
# Hardcoding rights:
# intellectualRights = EML::eml$intellectualRights(
# EML::eml$para("This work is licensed under a Creative Commons Attribution
# 4.0 International License.")

# Add the your text here stuff for the template
# UUIDgenerate for the unique identifier - put in the function call, if NULL,
# user has to supply one.
# The headings could be confusing because where do i write and where don't i
# write. Like directly under description header - you aren't actually supposed
# to write there.
# Could add some informational stuff to the template like under data resource
# main header, some info and background on this, and links etc. Information that
# is useful to the user - these can be indicated as italics or something so they
# know it won't go into the template.
# test multiple contact parsing
EML::eml_validate("./eml_test.xml")
