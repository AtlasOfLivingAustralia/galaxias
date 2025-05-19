test_that("build_archive() fails when file name not given", {
  # set up
  current_wd <- here::here()
  temp_dir <- withr::local_tempdir()
  usethis::local_project(temp_dir, force = TRUE)
  
  # tests
  build_archive() |>
    expect_error("Argument `file` is missing")
  
  # clean up
  unlink(temp_dir)  
})

test_that("build_archive() fails when file name doesn't end in `.zip`", {
  # set up
  current_wd <- here::here()
  temp_dir <- withr::local_tempdir()
  usethis::local_project(temp_dir, force = TRUE)
  
  # tests
  build_archive(file = "dwca.csv") |>
    expect_error("`file` must specify a file name ending with")
  
  # clean up
  unlink(temp_dir)  
})

test_that("build_archive() fails when can't find directory", {
  # set up
  current_wd <- here::here()
  temp_dir <- withr::local_tempdir()
  usethis::local_project(temp_dir, force = TRUE)

  # tests
  build_archive(file = "dwca.zip", quiet = TRUE) |>
    expect_error("Directory")
  
  # clean up
  unlink(temp_dir)
})

test_that("build_archive() fails when specified directory is missing all files", {
  # set up
  current_wd <- here::here()
  temp_dir <- withr::local_tempdir()
  usethis::local_project(temp_dir, force = TRUE)
  usethis::use_directory("data-publish")
  
  # tests
  build_archive(file = "dwca.zip", quiet = TRUE) |>
    expect_error("No files found")
  
  # clean up
  unlink("data-publish")
  unlink(temp_dir)
})

test_that("build_archive() works, respecting `overwrite`", {
  # set up
  current_wd <- here::here()
  temp_dir <- withr::local_tempdir()
  usethis::local_project(temp_dir, force = TRUE)
  usethis::use_directory("data-publish")
  use_metadata_template(quiet = TRUE)
  use_metadata(file = "metadata.Rmd",
               quiet = TRUE)
  df <- tibble::tibble(
    decimalLatitude = c(44.4, 44.4)
  ) |>
    dplyr::mutate(
      occurrenceID = random_id()
    ) |>
    write.csv("data-publish/occurrences.csv")
  use_schema(quiet = TRUE)
  
  # test building first time works
  file_out <- "dwca.zip"
  build_archive(file = file_out, quiet = TRUE) |>
    expect_no_error()
  file.exists(file_out) |>
    expect_true()
  
  # overwrite should fail
  build_archive(file = file_out, quiet = TRUE) |>
    expect_error()
  
  # this can be overidden 
  build_archive(file = file_out, overwrite = TRUE, quiet = TRUE) |>
    expect_no_error()
  
  # clean up
  unlink(file_out)
  unlink("metadata.Rmd")
  unlink("data-publish")
  unlink(temp_dir)
})

test_that("build_archive() messages work", {
  # set up
  current_wd <- here::here()
  temp_dir <- withr::local_tempdir()
  usethis::local_project(temp_dir, force = TRUE)
  usethis::use_directory("data-publish")
  use_metadata_template(quiet = TRUE)
  use_metadata(file = "metadata.Rmd",
               quiet = TRUE)
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
    build_archive(file = "dwca.zip")
  }
  msgs <- fix_filenames(fix_times(capture_cli_messages(build_archive_messages(1))))
  expect_snapshot(msgs)
  
  # clean up
  unlink("metadata.Rmd")
  unlink("data-publish")
  unlink("dwca.zip")
  unlink(temp_dir)
})


test_that("build_archive() menu appears", {
  # set up
  current_wd <- here::here()
  temp_dir <- withr::local_tempdir()
  usethis::local_project(temp_dir, force = TRUE)
  usethis::use_directory("data-publish")
  use_metadata_template(quiet = TRUE)
  use_metadata(file = "metadata.Rmd",
               quiet = TRUE)
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
    build_archive(file = "dwca.zip")
  }
  msgs <- fix_filenames(fix_times(capture_cli_messages(build_archive_with_mock(1))))
  expect_snapshot(msgs)
  
  # clean up
  unlink("metadata.Rmd")
  unlink("data-publish")
  unlink("dwca.zip")
  unlink(temp_dir)
})

test_that("build_archive() builds schema when missing", {
  # set up
  current_wd <- here::here()
  temp_dir <- withr::local_tempdir()
  usethis::local_project(temp_dir, force = TRUE)
  usethis::use_directory("data-publish")
  use_metadata_template(quiet = TRUE)
  use_metadata(file = "metadata.Rmd",
               quiet = TRUE)
  df <- tibble::tibble(
    decimalLatitude = c(44.4, 44.4)
  ) |>
    dplyr::mutate(
      occurrenceID = random_id()
    ) |>
    write.csv("data-publish/occurrences.csv")
  
  # tests
  archive_name <- "dwca.zip"
  build_archive(file = archive_name, 
                quiet = TRUE) |>
    expect_no_error()
  expect_in(archive_name, list.files())
  expect_in("meta.xml", list.files("data-publish"))
  
  # clean up
  unlink("metadata.Rmd")
  unlink("data-publish")
  unlink(archive_name)
  unlink(temp_dir)
})


test_that("build_archive() fails when missing data", {
  # set up
  current_wd <- here::here()
  temp_dir <- withr::local_tempdir()
  usethis::local_project(temp_dir, force = TRUE)
  usethis::use_directory("data-publish")
  use_metadata_template(quiet = TRUE)
  use_metadata(file = "metadata.Rmd",
               quiet = TRUE)
  
  # tests
  build_archive(file = "dwca.zip", 
                quiet = TRUE) |>
    expect_error()
  
  # clean up
  unlink("metadata.Rmd")
  unlink("data-publish")
  unlink(temp_dir)
})

test_that("build_archive() fails when missing metadata", {
  # set up
  current_wd <- here::here()
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
  build_archive(file = "dwca.zip", quiet = TRUE) |>
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
