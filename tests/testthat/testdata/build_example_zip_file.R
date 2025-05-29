# This script builds a zip file for testing `check_archive()`
# It has been added to Rbuildignore so it doesn't ship with the package
# Use this script to build zip files that include specific features (and bugs?)

# set up
devtools::load_all()
usethis::use_directory("TEMPORARY")
galaxias_config(directory = "TEMPORARY",
                archive = here::here("tests", "testthat", "testdata", "simple_dwca.zip"))

# add data to temporary location
use_metadata_template(quiet = TRUE)
use_metadata("metadata.Rmd", quiet = TRUE)
df <- tibble::tibble(
  decimalLatitude = c(44.4, 44.4)
) |>
  dplyr::mutate(
    occurrenceID = random_id()
  )
use_data_occurrences(df)
build_archive(quiet = TRUE)

# clean up
unlink("metadata.Rmd")
unlink("TEMPORARY", recursive = TRUE)
