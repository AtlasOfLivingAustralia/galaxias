test_that("switch_check() works", {
  expect_error(switch_check("abort", "something"))
  expect_warning(switch_check("warn", "something"))
  expect_message(switch_check("inform", "something"))
  expect_message(switch_check()) # empty message when no args given
})

test_that("check_data_frame() works", {
  # case with no errors
  x <- tibble(variable = c(1, 2))
  expect_no_error(check_data_frame(x))
  result <- check_data_frame(x)
  expect_equal(x, result)
  # case with errors
  x <- list(variable = c(1, 2))
  check_data_frame(x) |>
    expect_error(regexp = "objects supplied to \`check_\` functions must be a \`tibble\` or \`data.frame\`")
  x <- tibble(variable = c(1, 2), 
              something = "a_value")
  check_data_frame(x) |>
    expect_error(regexp = "\`data.frame\`s supplied to \`check_\` functions should only have one column.")
})

test_that("check_contains() works", {
  # case with no errors
  x <- tibble(variable = c(1, 2))
  y <- c(1, 2, 3)
  expect_no_error(check_contains(x, y))
  result <- check_contains(x, y)
  expect_equal(x, result) # i.e. check_ returns an unmodified tibble
  # with errors
  y <- c(2, 3)
  expect_error(
    check_contains(x, y, level = "abort"),
    regexp = "\`variable\` contains unexpected value\\(s\\)\\: 1")
  expect_warning(check_contains(x, y, level = "warn"))
  expect_message(check_contains(x, y, level = "inform"))
  expect_message(check_contains(x, y)) # defaults to "inform"
})

test_that("check_is_numeric() works", {
  # case with no errors
  x <- tibble(variable = as.double(c(1, 3)))
  expect_no_error(check_is_numeric(x))
  x <- tibble(variable = as.integer(c(1, 3)))
  result <- check_is_numeric(x)
  expect_equal(x, result)
  # with errors
  x <- tibble(variable = "a_string")
  expect_error(check_is_numeric(x, level = "abort"),
               regexp = "\`variable\` is not numeric")
  expect_warning(check_is_numeric(x, level = "warn"))
  expect_message(check_is_numeric(x, level = "inform"))
  expect_message(check_is_numeric(x))
})

test_that("check_is_string() works", {
  # with no errors
  x <- tibble(basisOfRecord = "something")
  expect_no_error(check_is_string(x))
  result <- check_is_string(x)
  expect_equal(x, result)
  # with errors
  x <- tibble(variable = as.double(c(1, 3)))
  expect_error(check_is_string(x, level = "abort"),
               regexp = "\`variable\` is not a string")
  expect_warning(check_is_string(x, level = "warn"))
  expect_message(check_is_string(x, level = "inform"))
  expect_message(check_is_string(x))
})

test_that("check_unique() works", {
  # with no errors
  x <- tibble(variable = c(1, 2, 3, 4))
  expect_no_error(check_unique(x))
  result <- check_unique(x)
  expect_equal(x, result)
  # with numeric errors
  x <- tibble(variable = c(1, 2, 2, 3, 4, 4))
  expect_error(check_unique(x, level = "abort"),
               regexp = "\`variable\` does not contain a unique value in each cell")
  # with string errors
  x <- tibble(variable = c("something", "something", "something_else"))
  expect_error(check_unique(x, level = "abort"),
               regexp = "\`variable\` does not contain a unique value in each cell")
  # other reporting levels
  expect_warning(check_unique(x, level = "warn"))
  expect_message(check_unique(x, level = "inform"))
  expect_message(check_unique(x))
})

test_that("check_within_range() works", {
  # with no errors
  x <- tibble(variable = c(1, 2, 3, 4, 5))
  expect_no_error(check_within_range(x, lower = 0, upper = 20) )
  result <- check_within_range(x, lower = 0, upper = 20)
  expect_equal(x, result)
  # with numeric errors
  expect_error(check_within_range(x, level = "abort", lower = 4, upper = 20),
               regexp = "\`variable\` contains values ouside of 4 <= x <= 20")
  expect_warning(check_within_range(x, level = "warn", lower = 4, upper = 20))
  expect_message(check_within_range(x, level = "inform", lower = 4, upper = 20))
  expect_message(check_within_range(x, lower = 4, upper = 20))
  # with string errors
  x <- tibble(variable = "something")
  expect_error(check_within_range(x, level = "abort", lower = 4, upper = 20),
               regexp = "\`variable\` is not numeric")
})