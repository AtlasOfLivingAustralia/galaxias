#' Function to create a new RStudio Project
#' 
#' Sets up all 'Biodiversity Data Package', which is a modified R package for
#' building Darwin Core Archives. Based heavily on usethis::create_tidy_package
#' @param path A path to the directory to create
#' @importFrom rlang is_interactive
#' @importFrom usethis create_package
#' @importFrom usethis local_project
#' @importFrom usethis proj_activate
#' @importFrom usethis proj_get
#' @importFrom usethis use_mit_license
#' @importFrom usethis use_testthat
#' @importFrom usethis use_tidy_description
#' @export
create_data_package <- function(path, 
                                copyright_holder = NULL,
                                open = rlang::is_interactive()){
  # Set up package via `usethis`
  # Use some - but not all - steps from `usethis::create_tidy_package()`
  create_package(path, rstudio = TRUE, open = FALSE)
  local_project(path)
  use_testthat()
  use_mit_license(copyright_holder)
  use_tidy_description()

  # Add necessary content to DESCRIPTION
  # add `usethis` to SUGGESTS (not DEPENDS, because code is only in data-raw)
  # add any packages mentioned in `data_manipulation_script.R` to SUGGESTS
  description <- readLines("DESCRIPTION")
  suggests_row <- which(grepl("^Suggests:", description))
  c(
    description[seq_len(suggests_row)],
    "    dplyr,",
    "    galaxias,",
    "    readr,",
    "    tibble,",
    "    usethis,",
    description[seq(suggests_row + 1, length(description))]
  ) |>
  writeLines(con = "DESCRIPTION")
  
  # Add README.md
  # Use a `galaxias`-specific README.Rmd instead of the `usethis` default
  # This requires replacing placeholder text with the supplied package name (`path`)
  system.file("./inst/data_package_files/README.Rmd", 
              package = "galaxias") |>
    readLines() |>
    gsub("`PKGNAME`", path, x = _) |>
    writeLines(con = "README.Rmd")

  # add `inst` with citation file
  dir.create("inst")
  system.file("./inst/data_package_files/CITATION", 
              package = "galaxias") |>
    readLines() |>
    gsub("`PKGNAME`", path, x = _) |>
    writeLines(con = "inst/CITATION")
  
  # Add a script to `data-raw` with example code of how to:
    # rename/select/relocate fields
    # save data out to `data` folder 
  dir.create("data-raw")
  dir.create("data") # populated by the user; but is where scripts point to from data-raw
  system.file("./inst/data_package_files/data_manipulation_script.R", 
              package = "galaxias") |>
    file.copy(to = "data-raw") |>
    invisible()
  
  # Add vignettes
  # - schema.md (converts to metadata.xml) - populates once `update_schema()` is called
  # - metadata.md (converts to eml.xml)
  # - report.md (builds an overview of the dataset, once data are populated)
  system.file("./inst/data_package_files/vignettes/metadata_statement.Rmd", 
              package = "galaxias") |>
    file.copy(to = "vignettes") |>
    invisible()
    
  # Add tests
  # option 1: this gets added later, once `data` is populated, using `update_tests()`
  # option 2: import whole test suite, and skip tests for non-available fields
  # leaning towards option 2, as at least all tests are then available for use
  
  # Launch
  proj_activate(proj_get())
}

# Data packages have three levels of licencing:
# - package licence (typically MIT)
# - data licencing (recommended CC-0)
# - record or image licencing (also recommended CC-0, but may not be)
# Meaning we do not use README or DESCRIPTION for DwCA licencing,
# but instead put a different file somewhere with meta.xml and eml.xml