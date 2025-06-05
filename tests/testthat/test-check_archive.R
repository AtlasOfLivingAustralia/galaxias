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

test_that("get_report() fails when objects of wrong class are given", {
  get_report(list()) |>
    expect_error(label = "Argument `obj` must be of class <character> or <gbif_validator>")
})

test_that("check_archive() fails when `filename` is `NULL`", {
  check_archive(filename = NULL) |>
    expect_error(label = "Argument `filename` must not be `NULL`")
})

test_that("check_archive() fails when `filename` is not a character", {
  check_archive(filename = 1L) |>
    expect_error(label = "Argument `filename` must inherit from class <character>")
})

test_that("check_archive() fails when `filename` doesn't end in `.zip`", {
  check_archive(filename = "something.csv") |>
    expect_error(label = "Argument `filename` must end in `.zip`")
})

test_that("check_archive() fails when `filename` doesn't exist", {
  check_archive(filename = "something.zip") |>
    expect_error(label = "Specified archive something.zip does not exist")
})

test_that("check_archive() fails when `username` is `NULL` (i.e. not specified", {
  # create a file to avoid the `exists` requirement in `filename`
  file.create("../TEST.zip") |> 
    invisible()
  # run check
  check_archive(filename = "TEST.zip",
                email = "hello@ala.org.au",
                password = "my-password") |>
    expect_error(label = "All GBIF credentials should be supplied.")
  unlink("../TEST.zip")
})

test_that("check_archive() fails when `username` is `NULL (i.e. not specified", {
  # create a file to avoid the `exists` requirement in `filename`
  file.create("../TEST.zip") |> 
    invisible()
  # run check
  check_archive(filename = "TEST.zip",
                username = 1L,
                email = "hello@ala.org.au",
                password = "my-password") |>
    expect_error(label = "All GBIF credentials should be supplied as strings.")
  unlink("../TEST.zip")
})

test_that("check_archive() fails when `username` is `NULL (i.e. not specified", {
  # create a file to avoid the `exists` requirement in `filename`
  file.create("../TEST.zip") |> 
    invisible()
  # run check
  check_archive(filename = "TEST.zip",
                username = c("username1", "username2"),
                email = "hello@ala.org.au",
                password = "my-password") |>
    expect_error(label = "All GBIF credentials should be length-1.")
  unlink("../TEST.zip")
})

test_that("check_archive() works", {
  skip_if_offline()
  
  # set up directory
  current_wd <- here::here()
  temp_dir <- withr::local_tempdir()
  usethis::local_project(temp_dir, force = TRUE)
  
  # build galaxias-specific content
  usethis::use_directory("data-publish")
  use_metadata_template(quiet = TRUE)
  use_metadata("metadata.Rmd", quiet = TRUE)
  df <- tibble::tibble(
    decimalLatitude = c(35.307, 35.307),
    decimalLongitude = c(149.125, 149.125)) |>
    dplyr::mutate(occurrenceID = random_id(),
                  .before = 1) |>
    write.csv("data-publish/occurrences.csv")
  use_schema(quiet = TRUE)
  build_archive(quiet = TRUE, overwrite = TRUE)
  
  # test `check_archive()`
  # check archive returns an object
  result <- check_archive("dwc-archive.zip", 
                          username = "atlasoflivingaustralia",
                          email = "ala4r@ala.org.au",
                          password = "galah-gbif-test-login",
                          wait = TRUE) |>
    expect_no_error()
  # check run was 'complete'
  is_gbif_validator_complete(result) |>
    expect_true()
  # check class
  inherits(result, "gbif_validator") |>
    expect_true()
  
  # test `get_report()`
  report1 <- get_report(result,
                        username = "atlasoflivingaustralia",
                        password = "galah-gbif-test-login") |>
    expect_no_error()
  report2 <- get_report(result$key,
                        username = "atlasoflivingaustralia",
                        password = "galah-gbif-test-login") |>
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