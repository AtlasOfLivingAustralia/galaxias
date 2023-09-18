#' Open a markdown template file for creating EML metadata
#' @param path File path where metadata template will be copied to
#' @param type Which template flavour to open (default: "ALA", more to come)
#' @param licence Numeric value corresponding to a valid licence to use for the
#' dataset. Read more [here:](https://support.ala.org.au/support/solutions/articles/6000197133-what-licence-should-i-use-)
#' #' \itemize{
#'   \item 1 = Creative Commons Zero - CC0
#'   \item 2 = Attribution – CC BY (4.0)
#'   \item 3 = Attribution-Noncommercial – CC BY-NC
#' @return Template file opened in RStudio for user to edit
#' @export
#' @importFrom usethis edit_file
edit_template <- function(path = ".",
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
  file.copy(path_to_md_template, ".")
  edit_file(paste0(path, "/user_template.md"))
}


#' Build and export an EML file using an input markdown template
#' Reference: EML package function `set_TextType()`
#' @param template Path to input markdown template
#' @param output_dir Path to output directory to save EML file. Defaults to
#' current working directly ("./eml.xml")
#' @param validate_eml Logical; whether to validate the EML file using the EML
#' package function `eml_validate()`
#' @return EML written to output directory
#' @export
#' @examples
#' build_eml(template = "./user_template.md")
#' build_eml(
#'   template = "./user_template.md",
#'   output_dir = "./metadata/my_eml.xml",
#'   validate = TRUE
#' )
build_eml <- function(template,
                      output_dir = "./eml.xml",
                      validate = FALSE) {
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

  # Assign markdown tags to relevant EML fields
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
      # Note: this is different to the reference of the actual dataset
      referencePublication = result_list$citation$para,
      intellectualRights = result_list$license$para,
      licensed = result_list$rights$para,
      creator = result_list$creator$para,
      contact = result_list$creator$para
      # electronicMailAddress
      # individualName
      # ?? == institutions
      # (Primary contacts)
      # ?? == primary-contact-name
      # ?? == primary-contact-email
      # additionalMetadata
    )
  )
  if (validate) {
    check_eml(my_eml)
  }
  validate_output_path(output_dir)
  message("Writing EML to file...\n")
  EML::write_eml(my_eml, output_dir)
}

#' Validate a user input output path / file name
#' @param filepath path to output file
#' @return NULL - console output. Stops if validation fails.
#' @keywords internal
validate_output_path <- function(filepath) {
  # Extract the directory from the filepath
  directory_name <- dirname(filepath)

  # Check if directory exists
  if (!dir.exists(directory_name)) {
    stop("\033[31mThe specified directory does not exist!\033[0m",
      call. = FALSE
    )
  }

  # Check if directory is writable
  if (file.access(directory_name, mode = 2) != 0) {
    stop("\033[31mThe specified directory is not writable!\033[0m",
      call. = FALSE
    )
  }

  # Check if file already exists (optional)
  if (file.exists(filepath)) {
    stop("\033[31mA file with the specified name already exists!\033[0m",
      call. = FALSE
    )
  }
}

# Notes and stuff
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
# Could also return the EML list object? would only make sense if also provide a
# function to input the list and then export EML. Easy to do.
# Check the UUIDgenerate is working
