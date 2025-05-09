
test_that("build_archive() fails when can't find directory", {
  # set up
  current_wd <- here::here()
  temp_dir <- withr::local_tempdir()
  usethis::local_project(temp_dir, force = TRUE)
  
  #tests
  build_archive(source = "random-folder", quiet = TRUE) |>
    expect_error("Directory")
  
  # clean up
  unlink("dwc-archive.zip")
  unlink(temp_dir)
})

test_that("build_archive() fails when missing all files in specified directory", {
  # set up
  current_wd <- here::here()
  temp_dir <- withr::local_tempdir()
  usethis::local_project(temp_dir, force = TRUE)
  usethis::use_directory("data-publish")
  
  #tests
  build_archive(quiet = TRUE) |>
    expect_message("Missing directory. Defaulting to") |>
    expect_error("No files found")
  
  # clean up
  unlink("data-publish")
  unlink("dwc-archive.zip")
  unlink(temp_dir)
})

test_that("build_archive() works with no arguments", {
  # set up
  current_wd <- here::here()
  temp_dir <- withr::local_tempdir()
  usethis::local_project(temp_dir, force = TRUE)
  usethis::use_directory("data-publish")
  use_metadata_template(quiet = TRUE)
  use_metadata(quiet = TRUE)
  df <- tibble::tibble(
    decimalLatitude = c(44.4, 44.4)
  ) |>
    dplyr::mutate(
      occurrenceID = random_id()
    ) |>
    write.csv("data-publish/occurrences.csv")
  use_schema(quiet = TRUE)
  
  
  #tests
  build_archive(quiet = TRUE) |>
    expect_no_error()
  expect_in("dwc-archive.zip", list.files(temp_dir))
  
  # clean up
  unlink("metadata.Rmd")
  unlink("data-publish")
  unlink("dwc-archive.zip")
  unlink(temp_dir)
})

test_that("build_archive() works messages work", {
  # set up
  current_wd <- here::here()
  temp_dir <- withr::local_tempdir()
  usethis::local_project(temp_dir, force = TRUE)
  usethis::use_directory("data-publish")
  use_metadata_template(quiet = TRUE)
  use_metadata(quiet = TRUE)
  df <- tibble::tibble(
    decimalLatitude = c(44.4, 44.4)
  ) |>
    dplyr::mutate(
      occurrenceID = random_id()
    ) |>
    write.csv("data-publish/occurrences.csv")
  use_schema(quiet = TRUE)
  
  
  #tests
  build_archive_messages <- function(x) {
    local_user_input(x)
    build_archive()
  }
  msgs <- fix_times(capture_cli_messages(build_archive_messages(1)))
  expect_snapshot(msgs)
  
  # clean up
  unlink("metadata.Rmd")
  unlink("data-publish")
  unlink("dwc-archive.zip")
  unlink(temp_dir)
})

test_that("build_archive() menu appears", {
  # set up
  current_wd <- here::here()
  temp_dir <- withr::local_tempdir()
  usethis::local_project(temp_dir, force = TRUE)
  usethis::use_directory("data-publish")
  use_metadata_template(quiet = TRUE)
  use_metadata(quiet = TRUE)
  df <- tibble::tibble(
    decimalLatitude = c(44.4, 44.4)
  ) |>
    dplyr::mutate(
      occurrenceID = random_id()
    ) |>
    write.csv("data-publish/occurrences.csv")
  
  
  #tests
  build_archive_with_mock <- function(x) {
    local_user_input(x)
    build_archive()
  }
  msgs <- capture_cli_messages(build_archive_with_mock(1))
  expect_snapshot(msgs)
  
  # clean up
  unlink("metadata.Rmd")
  unlink("data-publish")
  unlink("dwc-archive.zip")
  unlink(temp_dir)
})

test_that("build_archive() builds schema when missing", {
  # set up
  current_wd <- here::here()
  temp_dir <- withr::local_tempdir()
  usethis::local_project(temp_dir, force = TRUE)
  usethis::use_directory("data-publish")
  use_metadata_template(quiet = TRUE)
  use_metadata(quiet = TRUE)
  df <- tibble::tibble(
    decimalLatitude = c(44.4, 44.4)
  ) |>
    dplyr::mutate(
      occurrenceID = random_id()
    ) |>
    write.csv("data-publish/occurrences.csv")
  
  
  #tests
  build_archive(quiet = TRUE) |>
    expect_no_error()
  expect_in("dwc-archive.zip", list.files(temp_dir))
  expect_in("meta.xml", list.files("data-publish"))
  
  # clean up
  unlink("metadata.Rmd")
  unlink("data-publish")
  unlink("dwc-archive.zip")
  unlink(temp_dir)
})


test_that("build_archive() fails when missing data", {
  # set up
  current_wd <- here::here()
  temp_dir <- withr::local_tempdir()
  usethis::local_project(temp_dir, force = TRUE)
  usethis::use_directory("data-publish")
  use_metadata_template(quiet = TRUE)
  use_metadata(quiet = TRUE)
  
  #tests
  build_archive(quiet = TRUE) |>
    expect_error()
  
  # clean up
  unlink("metadata.Rmd")
  unlink("data-publish")
  unlink("dwc-archive.zip")
  unlink(temp_dir)
})

test_that("build_archive() fails when missing metadata", {
  # set up
  current_wd <- here::here()
  temp_dir <- withr::local_tempdir()
  usethis::local_project(temp_dir, force = TRUE)
  usethis::use_directory("data-publish")
  # use_metadata_template(quiet = TRUE)
  # use_metadata(quiet = TRUE)
  df <- tibble::tibble(
    decimalLatitude = c(44.4, 44.4)
  ) |>
    dplyr::mutate(
      occurrenceID = random_id()
    ) |>
    write.csv("data-publish/occurrences.csv")
  
  #tests
  build_archive(quiet = TRUE) |>
    expect_error()
  
  # clean up
  unlink("metadata.Rmd")
  unlink("data-publish")
  unlink("dwc-archive.zip")
  unlink(temp_dir)
})
