test_that("check_directory() works with no arguments", {
  skip_if_offline() 
  # note that delma::check_metadata() requires an internet connection to 
  # perform well; meaning this function cannot be safely be called when offline
  
  # set up
  temp_dir <- withr::local_tempdir()
  usethis::local_project(temp_dir, force = TRUE)
  usethis::use_directory("data-publish")
  use_metadata_template(quiet = TRUE)
  use_metadata("metadata.Rmd", quiet = TRUE)
  df_occ <- tibble::tibble(
    decimalLatitude = c(44.4, 44.4)
  ) |>
    dplyr::mutate(
      occurrenceID = random_id()
    ) |>
    write.csv("data-publish/occurrences.csv")
  df_events <- tibble::tibble(
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

  # clean up
  unlink("../dwc-archive.zip")
  unlink("metadata.Rmd")
  unlink("data-publish")
  unlink(temp_dir)
})
