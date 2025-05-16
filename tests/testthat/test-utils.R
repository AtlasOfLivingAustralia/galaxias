
# ---- tests below are from the gargle package -------------------- #
# https://github.com/r-lib/gargle/blob/main/tests/testthat/test-utils-ui.R

test_that("cli_menu() basic usage", {
  cli_menu_with_mock <- function(x) {
    local_user_input(x)
    cli_menu(
      "Found multiple thingies.",
      "Which one do you want to use?",
      glue::glue("label {head(letters, 3)}")
    )
  }
  
  expect_snapshot(cli_menu_with_mock(1))
})

test_that("cli_menu() does not infinite loop with invalid mocked input", {
  cli_menu_with_mock <- function(x) {
    local_user_input(x)
    cli_menu(
      "Found multiple thingies.",
      "Which one do you want to use?",
      glue::glue("label {head(letters, 3)}")
    )
  }
  
  expect_snapshot(cli_menu_with_mock("nope"), error = TRUE)
})

test_that("cli_menu() can work through multiple valid mocked inputs", {
  cli_menu_with_mock <- function(x) {
    local_user_input(x)
    header <- "Found multiple thingies."
    prompt <- "Which one do you want to use?"
    choices <- glue::glue("label {1:3}")
    first <- cli_menu(header, prompt, choices)
    second <- cli_menu(header, prompt, choices)
    c(first, second)
  }
  
  expect_snapshot(
    out <- cli_menu_with_mock(c(1, 3))
  )
  expect_equal(out, c(1, 3))
})

test_that("cli_menu(), request exit via 0", {
  cli_menu_with_mock <- function(x) {
    local_user_input(x)
    cli_menu(
      "Found multiple thingies.",
      "Which one do you want to use?",
      glue::glue("label {head(letters, 3)}")
    )
  }
  
  expect_snapshot(error = TRUE, cli_menu_with_mock(0))
})

test_that("cli_menu(exit =) works", {
  cli_menu_with_mock <- function(x) {
    local_user_input(x)
    cli_menu(
      header = "Hey we need to talk.",
      prompt = "What do you want to do?",
      choices = c(
        "Give up",
        "Some other thing"
      ),
      exit = 1
    )
  }
  
  expect_snapshot(error = TRUE, cli_menu_with_mock(1))
  expect_snapshot(cli_menu_with_mock(2))
})

test_that("check_file_argument() errors when `file` is `NULL`", {
  check_file_argument(file = NULL) |>
    expect_error("Argument `file` is missing")
})

test_that("check_file_argument() errors when `file` is not a character", {
  check_file_argument(file = 3L) |>
    expect_error("Argument `file` must be of class `character`")
})

test_that("check_file_argument() errors when `file` does not exist", {
  check_file_argument(file = "nothing.xlsx") |>
    expect_error("File")
})

test_that("check_file_argument() does not error when `must_exist` = FALSE", {
  check_file_argument(file = "nothing.xlsx",
                      must_exist = FALSE) |>
    expect_no_error()
})

test_that("check_publish_directory() performs correctly", {
  skip("Tests not ready")
})