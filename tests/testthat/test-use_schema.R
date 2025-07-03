
test_that("use_schema() works with no arguments", {
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
  
  #tests
  use_schema() |>
    expect_message()
  expect_length(list.files("data-publish"), 2)
  expect_in(c("meta.xml",
              "occurrences.csv"), 
            list.files("data-publish")
            )
  
  # clean up
  unlink("data-publish/occurrences.csv")
  unlink(temp_dir)
})

test_that("use_schema() fails when no matching DwC csv files exist in directory", {
  # set up
  temp_dir <- withr::local_tempdir()
  usethis::local_project(temp_dir, force = TRUE)
  
  #tests
  use_schema(quiet = TRUE) |>
    expect_error("Must include")

  # clean up
  unlink(temp_dir)
})
