
test_that("check_directory() fails when galaxias_config does not contain an existing folder.", {
  # set up
  current_wd <- here::here()
  temp_dir <- withr::local_tempdir()
  usethis::local_project(temp_dir, force = TRUE)
  galaxias_config(directory = glue::glue("a-random-name"))
  
  # tests
  check_directory() |>
    expect_error("Must supply a source folder name") # this error message will change
  
  # clean up
  galaxias_config(directory = "data-publish")
  unlink(temp_dir)
})

test_that("check_directory() works with no arguments", {
  # set up
  current_wd <- here::here()
  temp_dir <- withr::local_tempdir()
  usethis::local_project(temp_dir, force = TRUE)
  galaxias_config(archive = glue::glue("{temp_dir}.zip"))
  usethis::use_directory("data-publish")
  use_metadata_template(quiet = TRUE)
  use_metadata("metadata.Rmd", quiet = TRUE)
  tibble::tibble(
    decimalLatitude = c(44.4, 44.4)
  ) |>
    dplyr::mutate(
      occurrenceID = random_id()
    ) |>
    write.csv("data-publish/occurrences.csv")
  tibble::tibble(
    decimalLatitude = c(44.4, 44.4)
  ) |>
    dplyr::mutate(
      eventID = random_id()
    ) |>
    write.csv("data-publish/events.csv")
  use_schema(quiet = TRUE)
  build_archive(quiet = TRUE) # a complete archive
  
  # tests
  check_directory_mock <- function() {
    check_directory()
  }
  msgs <- check_directory_mock() |>
    capture_cli_messages() |>
    fix_times() |>
    fix_filenames() |>
    fix_duplicates()
  expect_snapshot(msgs)
  archive_name <- glue::glue("{basename(temp_dir)}.zip")
  
  # clean up
  galaxias_config(archive = glue::glue("{here::here()}.zip"))
  unlink(glue::glue("../{archive_name}"))
  unlink("metadata.Rmd")
  unlink("data-publish")
  unlink(temp_dir)
})
