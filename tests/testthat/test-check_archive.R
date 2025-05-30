test_that("view_report() fails when no arguments are supplied", {
  view_report() |>
    expect_error(label = "Please supply an object to `view_report()`")
})

test_that("view_report() fails when object of incorrect class is supplied", {
  view_report("a text string") |>
    expect_error(label = "Argument `x` must have class `gbif_response`.")
})

test_that("get_report() fails when no objects are given", {
  get_report() |>
    expect_error(label = "Please provide one of either `key` or `response`")
})

test_that("check_archive() works", {
  skip_if_offline()
  
  # set up directory
  current_wd <- here::here()
  temp_dir <- withr::local_tempdir()
  usethis::local_project(temp_dir, force = TRUE)
  
  # set gbif credentials
  galaxias_config(
    gbif = list(
      username = "atlasoflivingaustralia",
      email = "ala4r@ala.org.au",
      password = "galah-gbif-test-login"
    ))
  
  # build galaxias-specific content
  usethis::use_directory("data-publish")
  use_metadata_template(quiet = TRUE)
  use_metadata("metadata.Rmd", quiet = TRUE)
  tibble::tibble(
    decimalLatitude = c(35.307, 35.307),
    decimalLongitude = c(149.125, 149.125)) |>
    dplyr::mutate(occurrenceID = random_id(),
                  .before = 1) |>
    write.csv("data-publish/occurrences.csv")
  use_schema(quiet = TRUE)
  build_archive(quiet = TRUE)
  
  # test `check_archive()`
  # check archive returns an object
  result <- check_archive(wait = TRUE) |>
    expect_no_error()
  # check run was 'complete'
  is_gbif_validator_complete(result) |>
    expect_true()
  # check class
  inherits(result, "gbif_validator") |>
    expect_true()
  
  # test `get_report()`
  report1 <- get_report(result) |>
    expect_no_error()
  report2 <- get_report(result$key)  |>
    expect_no_error()
  expect_identical(report1, report2)
  
  # test `view_report()`
  view_report(result) # how to test this print 'correctly'?
  
  # clean up
  unlink("../dwc-archive.zip")
  unlink("metadata.Rmd")
  unlink("data-publish")
  unlink(temp_dir)
})

test_that("retrieving a processed query works", {
  skip_if_offline()
  skip("Tests not ready")
  get_report("8e57e026-934e-49ec-b8bb-9b2af1f11215")
})