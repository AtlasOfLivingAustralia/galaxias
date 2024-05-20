test_that("switch_check() works", {
  expect_error(switch_check("abort", "something"))
  expect_warning(switch_check("warn", "something"))
  expect_message(switch_check("inform", "something"))
  expect_message(switch_check()) # empty message when no args given
})

test_that("check_contains() works", {
  x <- c(1, 2)
  y <- c(1, 2, 3)
  result <- check_contains(x, y) # invisibly returns TRUE
  expect_true(result)
  expect_error(check_contains(y, x, level = "abort"))
  expect_warning(check_contains(y, x, level = "warn"))
  expect_message(check_contains(y, x, level = "inform"))
  expect_message(check_contains(y, x)) # defaults to "inform"
})

test_that("check_is_numeric() works", {
  check_is_numeric(as.double(3))
  result <- check_is_numeric(as.integer(3))
  expect_true(result)
  x <- "a_string"
  expect_error(check_is_numeric(x, level = "abort"))
  expect_warning(check_is_numeric(x, level = "warn"))
  expect_message(check_is_numeric(x, level = "inform"))
  expect_message(check_is_numeric(x)) # defaults to "inform"
})

test_that("check_is_string() works", {
  result <- check_is_string("a_string")
  expect_true(result)
  x <- 3
  expect_error(check_is_string(x, level = "abort"))
  expect_warning(check_is_string(x, level = "warn"))
  expect_message(check_is_string(x, level = "inform"))
  expect_message(check_is_string(x)) # defaults to "inform"
})

test_that("check_unique() works", {
  x <- c(1, 2, 3, 4)
  result <- check_unique(x) # invisibly returns TRUE
  expect_true(result)
  x <- c(1, 1, 2, 2, 3, 4, 5)
  expect_error(check_unique(x, level = "abort"))
  expect_warning(check_unique(x, level = "warn"))
  expect_message(check_unique(x, level = "inform"))
  expect_message(check_unique(x)) # defaults to "inform"
})

test_that("check_within_range() works", {
  x <- c(1, 2, 3, 4, 5)
  result <- check_within_range(x, lower = 0, upper = 20) # invisibly returns TRUE
  expect_true(result)
  expect_error(check_within_range(x, level = "abort", lower = 4, upper = 20))
  expect_warning(check_within_range(x, level = "warn", lower = 4, upper = 20))
  expect_message(check_within_range(x, level = "inform", lower = 4, upper = 20))
  expect_message(check_within_range(x, lower = 4, upper = 20)) # defaults to "inform"
})