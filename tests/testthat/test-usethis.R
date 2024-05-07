test_that("`usethis` functions generate correct inputs", {
  testdir <- tempdir() # this is per-session; do not `unlink()`
  pkg <- paste0(testdir, "/testpackage") # this is the test package
  
  # test default files are added correctly
  create_bd_package(pkg, open = FALSE)
  pkg_files <- list.files(pkg, 
                          pattern = "[[:alpha:]]+", # only file names containing letters
                          all.files = TRUE)
  expect_contains(pkg_files,
                  c(".gitignore",
                    "data",
                    "data-raw",
                    "DESCRIPTION",
                    "README.Rmd",
                    "testpackage.Rproj"))
  glue("{pkg}/data-raw/data_manipulation_script.R") |> expect_exists()
  ## option to add tests to ensure relevant content is populated here
  ## e.g. to DESCRIPTION and README
  
  # test mandatory usethis functions
  usethis::local_project(pkg, force = TRUE)
  ## add data
  occurrences <- tibble::tibble(occurrenceID = "123456",
                                decimalLatitude = 0.1,
                                decimalLongitude = 5,
                                scientificName = "Something something")
  use_bd_data(occurrences)
  glue("{pkg}/data/occurrences.rda") |> expect_exists()
  
  ## add schema
  
  ## add metadata
  
  
  # test optional usethis functions
  ## add tests
  use_bd_testthat()
  glue("{pkg}/tests") |> expect_exists()
  glue("{pkg}/tests/testthat.R") |> expect_exists()
  glue("{pkg}/tests/testthat") |> expect_exists()
  glue("{pkg}/tests/testthat/test-decimalLatitude_decimalLongitude.R") |> expect_exists()
  ## add report
  
  # tidy up
  unlink(pkg, recursive = TRUE)
})