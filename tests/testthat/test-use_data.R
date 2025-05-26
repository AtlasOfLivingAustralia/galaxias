
test_that("use_data() fails when no data is supplied", {
  # set up
  current_wd <- here::here()
  temp_dir <- withr::local_tempdir()
  usethis::local_project(temp_dir, force = TRUE)
  
  #tests
  use_data(quiet = TRUE) |>
    expect_error()
  
  # clean up
  unlink(temp_dir)
})

test_that("use_data() fails when supplied object isn't a data.frame/tibble", {
  # set up
  current_wd <- here::here()
  temp_dir <- withr::local_tempdir()
  usethis::local_project(temp_dir, force = TRUE)
  string <- "how ya doin'"
  
  #tests
  use_data(string, quiet = TRUE) |>
    expect_error()
  
  # clean up
  unlink(temp_dir)
})

test_that("use_data() fails when supplied object isn't real", {
  # set up
  current_wd <- here::here()
  temp_dir <- withr::local_tempdir()
  usethis::local_project(temp_dir, force = TRUE)
  df_occ <- tibble::tibble(
    occurrenceID = c("123s", "123f"),
    decimalLatitude = c(-25, -25)
  )
  df_events <- tibble::tibble(
    eventID = c("123s", "123f"),
    decimalLatitude = c(-25, -25)
  )
  
  #tests
  use_data(non_existant_object, quiet = TRUE) |>
    expect_error()
  use_data(c(df_occ, df_events), quiet = TRUE) |>
    expect_error("Can only save existing") # uncertain whether this should be expected behaviour
  
  # clean up
  unlink(temp_dir)
})

test_that("use_data() fails when supplied more than one data.frame/tibble", {
  # set up
  current_wd <- here::here()
  temp_dir <- withr::local_tempdir()
  usethis::local_project(temp_dir, force = TRUE)
  df_occ <- tibble::tibble(
    occurrenceID = c("123s", "123f"),
    decimalLatitude = c(-25, -25)
  )
  df_events <- tibble::tibble(
    eventID = c("123s", "123f"),
    decimalLatitude = c(-25, -25)
  )
  
  #tests
  use_data(df_occ, df_events, quiet = TRUE) |>
    expect_error()
  
  # clean up
  unlink(temp_dir)
})

test_that("check_data_type() identifies occurrences/events correctly", {
  # set up
  df_occ <- tibble::tibble(
    occurrenceID = c("123s", "123f"),
    decimalLatitude = c(-25, -25)
  )
  df_events1 <- tibble::tibble(
    eventID = c("123s", "123f"),
    decimalLatitude = c(-25, -25)
  )
  df_events2 <- tibble::tibble(
    parentEventID = c("123s", "123f"),
    decimalLatitude = c(-25, -25)
  )
  
  #tests
  type <- check_data_type(df_occ)
  expect_equal(type, "occurrence")
  type <- check_data_type(df_events1)
  expect_equal(type, "event")
  type <- check_data_type(df_events2)
  expect_equal(type, "event")
})

test_that("use_data() works with no arguments", {
  # set up
  current_wd <- here::here()
  temp_dir <- withr::local_tempdir()
  usethis::local_project(temp_dir, force = TRUE)
  df_occ <- tibble::tibble(
    decimalLatitude = c(44.4, 44.4)
  ) |>
    dplyr::mutate(
      occurrenceID = random_id()
    )
  df_event <- tibble::tibble(
      decimalLatitude = c(44.4, 44.4)
    ) |>
    dplyr::mutate(
      eventID = random_id()
    )
  
  #tests
  df_occ |> use_data(quiet = TRUE)
  expect_length(list.files("data-publish"), 1)
  expect_in("occurrences.csv", list.files("data-publish"))
  
  df_event |> use_data(quiet = TRUE)
  expect_length(list.files("data-publish"), 2)
  expect_in("events.csv", list.files("data-publish"))
  
  # clean up
  unlink(temp_dir)
})

test_that("use_data() messages work", {
  # set up
  current_wd <- here::here()
  temp_dir <- withr::local_tempdir()
  usethis::local_project(temp_dir, force = TRUE)
  df <- tibble::tibble(
    decimalLatitude = c(44.4, 44.4)
  ) |>
    dplyr::mutate(
      occurrenceID = random_id()
    )
  
  #tests
  use_data_messages <- function(x, y, df) {
    local_user_input(c(x, y))
    df |> use_data()
  }
  msgs <- fix_filenames(fix_times(capture_cli_messages(use_data_messages(1, 1, df))))
  expect_snapshot(msgs)
  expect_length(list.files("data-publish"), 1)
  expect_in("occurrences.csv", list.files("data-publish"))
  
  # clean up
  unlink("data-publish")
  unlink(temp_dir)
})


test_that("use_data_occurrences() & use_data_events() fail if supplied object isn't a data.frame/tibble", {
  # set up
  current_wd <- here::here()
  temp_dir <- withr::local_tempdir()
  usethis::local_project(temp_dir, force = TRUE)
  string <- "a string"
  
  #tests
  string |> use_data_occurrences(quiet = TRUE) |>
    expect_error("Must provide a `tibble`/`data.frame`.")
  string |> use_data_events(quiet = TRUE) |>
    expect_error("Must provide a `tibble`/`data.frame`.")
  
  
  # clean up
  unlink(temp_dir)
})


test_that("use_data_occurrences() works with no arguments", {
  # set up
  current_wd <- here::here()
  temp_dir <- withr::local_tempdir()
  usethis::local_project(temp_dir, force = TRUE)
  df <- tibble::tibble(
    decimalLatitude = c(44.4, 44.4)
  ) |>
    dplyr::mutate(
      occurrenceID = random_id()
    )
  
  #tests
  df |> use_data_occurrences(quiet = TRUE)
  expect_length(list.files("data-publish"), 1)
  expect_in("occurrences.csv", list.files("data-publish"))
  
  # clean up
  unlink(temp_dir)
})

test_that("use_data_events() works with no arguments", {
  # set up
  current_wd <- here::here()
  temp_dir <- withr::local_tempdir()
  usethis::local_project(temp_dir, force = TRUE)
  df <- tibble::tibble(
    decimalLatitude = c(44.4, 44.4)
  ) |>
    dplyr::mutate(
      eventID = random_id()
    )
  
  #tests
  df |> use_data_events(quiet = TRUE)
  expect_length(list.files("data-publish"), 1)
  expect_in("events.csv", list.files("data-publish"))
  
  # clean up
  unlink(temp_dir)
})


test_that("use_data() overwrites file when overwrite = TRUE", {
  # set up
  current_wd <- here::here()
  temp_dir <- withr::local_tempdir()
  usethis::local_project(temp_dir, force = TRUE)
  df <- tibble::tibble(
    decimalLatitude = c(44.4, 44.4)
  ) |>
    dplyr::mutate(
      occurrenceID = random_id()
    )
  use_data(df, quiet = TRUE) # add occurrences.csv
  
  # tests
  timestamp_1 <- R.utils::lastModified("data-publish/occurrences.csv")
  use_data(df, quiet = TRUE) |> 
    expect_message() # didn't overwrite
  Sys.sleep(2)
  df <- df |> dplyr::mutate(basisOfRecord = "humanObservation") # update df
  use_data(df, overwrite = TRUE, quiet = TRUE)
  timestamp_2 <- R.utils::lastModified("data-publish/occurrences.csv")
  expect_true(timestamp_2 > timestamp_1)
  
  # clean up
  unlink(temp_dir)
})

# FIXME: This doesn't work. 
#        Although it runs, It also exits testthat automation, and no test is registered.
# test_that("use_data() suggests specific use_data functions when guess is wrong", {
#   skip_on_cran()
#   skip_on_ci()
#   # set up
#   current_wd <- here::here()
#   temp_dir <- withr::local_tempdir()
#   usethis::local_project(temp_dir, force = TRUE)
#   df <- tibble::tibble(
#     decimalLatitude = c(44.4, 44.4)
#   ) |>
#     dplyr::mutate(
#       occurrenceID = random_id()
#     )
# 
#   #tests
#   use_data_messages <- function(x, df) {
#     local_user_input(x)
#     df |> use_data()
#   }
#   # msgs <- fix_filenames(fix_times(capture_cli_messages(use_data_messages(2, df))))
#   use_data_messages(2, df) |>
#     expect_message("To specify data type")
#   # expect_snapshot(msgs)
# 
#   # clean up
#   unlink("data-publish")
#   unlink(temp_dir)
# })

