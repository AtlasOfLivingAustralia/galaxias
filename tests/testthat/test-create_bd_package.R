test_that("`usethis` functions generate a package", {
  testdir <- tempdir() # this is per-session; do not `unlink()`
  pkg <- paste0(testdir, "/testpackage") # this is the test package
  galaxias_repo <- getwd()
  
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
                    "metadata.md",
                    "testpackage.Rproj"))
  glue("{pkg}/data-raw/data_manipulation_script.R") |> 
    file.exists() |>
    expect_true()
  ## option to add tests to ensure relevant content is populated here
  ## e.g. to DESCRIPTION and README
  
  # test mandatory usethis functions
  usethis::local_project(pkg, force = TRUE)
  ## add data
  occurrences <- tibble::tibble(occurrenceID = "123456",
                                decimalLatitude = 0.1,
                                decimalLongitude = 5,
                                scientificName = "Something something")
  usethis::use_data(occurrences, internal = FALSE)
  glue("{pkg}/data/occurrences.rda") |> 
    file.exists() |>
    expect_true()
  
  ## add schema
  
  
  # test optional usethis functions
  ## add tests
  use_bd_testthat()
  file_list <- c(
    glue("{pkg}/tests"),
    glue("{pkg}/tests/testthat.R"),
    glue("{pkg}/tests/testthat"),
    glue("{pkg}/tests/testthat/test-decimalLatitude_decimalLongitude.R"))
  file.exists(file_list) |>
    all() |>
    expect_true()
  
  # tidy up
  unlink(pkg, recursive = TRUE)
  setwd(galaxias_repo)
})