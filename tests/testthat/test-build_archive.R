test_that("build_archive() fails when `/data-publish` is missing", {
  build_archive() |>
    expect_error(label = "Directory data-publish does not exist.")
})

test_that("build_archive() fails when `file` is `NULL`", {
  build_archive(file = NULL) |>
    expect_error(label = "Argument `file` must not be `NULL`")
})

test_that("build_archive() fails when `file` is not a character", {
  build_archive(file = 1L) |>
    expect_error(label = "Argument `file` must inherit from class <character>")
})

test_that("build_archive() fails when `file` doesn't end in `.zip`", {
  build_archive(file = "something.csv") |>
    expect_error(label = "Argument `file` must end in `.zip`")
})

test_that("build_archive() fails when specified directory is missing all files", {
  # set up
  temp_dir <- withr::local_tempdir()
  usethis::local_project(temp_dir, force = TRUE)
  usethis::use_directory("data-publish")
  
  # tests
  build_archive(quiet = TRUE) |>
    expect_error("No files found in")
  
  # clean up
  unlink("data-publish")
  unlink(temp_dir)
})

test_that("build_archive() works with no arguments", {
  # set up
  temp_dir <- withr::local_tempdir()
  usethis::local_project(temp_dir, force = TRUE)
  usethis::use_directory("data-publish")
  use_metadata_template(quiet = TRUE)
  use_metadata("metadata.Rmd", quiet = TRUE)
  df <- tibble::tibble(
    decimalLatitude = c(44.4, 44.4)
  ) |>
    dplyr::mutate(
      occurrenceID = random_id()
    ) |>
    write.csv("data-publish/occurrences.csv")
  use_schema(quiet = TRUE)
  
  # tests
  build_archive(quiet = TRUE) |>
    expect_no_error()
  expect_in("dwc-archive.zip", 
            list.files(".."))
  expect_in(c("occurrences.csv", "eml.xml", "meta.xml"), 
            zip::zip_list("../dwc-archive.zip")$filename) # correct files in archive
  
  # clean up
  unlink("../dwc-archive.zip")
  unlink("metadata.Rmd")
  unlink("data-publish")
  unlink(temp_dir)
})

test_that("build_archive() messages work", {
  # set up
  temp_dir <- withr::local_tempdir()
  usethis::local_project(temp_dir, force = TRUE)
  usethis::use_directory("data-publish")
  use_metadata_template(quiet = TRUE)
  use_metadata("metadata.Rmd", quiet = TRUE)
  df <- tibble::tibble(
    decimalLatitude = c(44.4, 44.4)
  ) |>
    dplyr::mutate(
      occurrenceID = random_id()
    ) |>
    write.csv("data-publish/occurrences.csv")
  use_schema(quiet = TRUE)
  
  # tests
  build_archive_messages <- function(x) {
    local_user_input(x)
    build_archive()
  }
  msgs <- build_archive_messages(1) |>
    capture_cli_messages() |>
    fix_times() |>
    fix_filenames() |>
    fix_duplicates()
  expect_snapshot(msgs)
  
  # clean up
  unlink("../dwc-archive.zip")
  unlink("metadata.Rmd")
  unlink("data-publish")
  unlink(temp_dir)
})


test_that("build_archive() menu appears", {
  # set up
  temp_dir <- withr::local_tempdir()
  usethis::local_project(temp_dir, force = TRUE)
  usethis::use_directory("data-publish")
  use_metadata_template(quiet = TRUE)
  use_metadata("metadata.Rmd", quiet = TRUE)
  df <- tibble::tibble(
    decimalLatitude = c(44.4, 44.4)
  ) |>
    dplyr::mutate(
      occurrenceID = random_id()
    ) |>
    write.csv("data-publish/occurrences.csv")
  
  # tests
  build_archive_with_mock <- function(x) {
    local_user_input(x)
    build_archive(overwrite = TRUE) # `overwrite` added to avoid ERRORS on macbuilder and winbuilder
  }
  
  msgs <- build_archive_with_mock(1) |>
    capture_cli_messages() |>
    fix_times() |>
    fix_filenames() |>
    fix_duplicates()
  expect_snapshot(msgs)

  # clean up
  unlink("../dwc-archive.zip")
  unlink("metadata.Rmd")
  unlink("data-publish")
  unlink(temp_dir)
})

test_that("build_archive() builds schema when missing", {
  # set up
  temp_dir <- withr::local_tempdir()
  usethis::local_project(temp_dir, force = TRUE)
  usethis::use_directory("data-publish")
  use_metadata_template(quiet = TRUE)
  use_metadata("metadata.Rmd", quiet = TRUE)
  df <- tibble::tibble(
    decimalLatitude = c(44.4, 44.4)
  ) |>
    dplyr::mutate(
      occurrenceID = random_id()
    ) |>
    write.csv("data-publish/occurrences.csv")
  
  # tests
  build_archive(overwrite = TRUE, # `overwrite` added to avoid ERRORS on macbuilder and winbuilder
                quiet = TRUE) |> 
    expect_no_error()
  expect_in("dwc-archive.zip", list.files(".."))
  expect_in("meta.xml", list.files("data-publish")) # in directory
  expect_in(c("occurrences.csv", "eml.xml", "meta.xml"), 
            zip::zip_list("../dwc-archive.zip")$filename) # correct files in archive
  
  # clean up
  unlink("../dwc-archive.zip")
  unlink("metadata.Rmd")
  unlink("data-publish")
  unlink(temp_dir)
})


test_that("build_archive() fails when missing data", {
  # set up
  temp_dir <- withr::local_tempdir()
  usethis::local_project(temp_dir, force = TRUE)
  usethis::use_directory("data-publish")
  use_metadata_template(quiet = TRUE)
  use_metadata("metadata.Rmd", quiet = TRUE)
  
  # tests
  build_archive(quiet = TRUE) |>
    expect_error()
  
  # clean up
  unlink("metadata.Rmd")
  unlink("data-publish")
  unlink(temp_dir)
})

test_that("build_archive() fails when missing metadata", {
  # set up
  temp_dir <- withr::local_tempdir()
  usethis::local_project(temp_dir, force = TRUE)
  usethis::use_directory("data-publish")
  df <- tibble::tibble(
    decimalLatitude = c(44.4, 44.4)
  ) |>
    dplyr::mutate(
      occurrenceID = random_id()
    ) |>
    write.csv("data-publish/occurrences.csv")
  
  # tests
  build_archive(quiet = TRUE) |>
    expect_error()
  
  # clean up
  unlink("metadata.Rmd")
  unlink("data-publish")
  unlink(temp_dir)
})

# test_that("build_archive() will use data-publish folder if it's there AND a named directory isn't found", {
#   # set up
#   current_wd <- here::here()
#   temp_dir <- withr::local_tempdir()
#   usethis::local_project(temp_dir, force = TRUE)
#   usethis::use_directory("data-publish")
#   use_metadata_template(quiet = TRUE)
#   use_metadata(quiet = TRUE)
#   df <- tibble::tibble(
#     decimalLatitude = c(44.4, 44.4)
#   ) |>
#     dplyr::mutate(
#       occurrenceID = random_id()
#     ) |>
#     write.csv("data-publish/occurrences.csv")
#   use_schema(quiet = TRUE)
#   
#   
#   # tests
#   build_archive(quiet = TRUE) |>
#     expect_no_error()
#   expect_in("dwc-archive.zip", list.files(temp_dir))
#   
#   # clean up
#   unlink("metadata.Rmd")
#   unlink("data-publish")
#   unlink("dwc-archive.zip")
#   unlink(temp_dir)
# })
