#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  wrapper
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  spatial validations
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#  Latitude ++++

test_that("validate_decimal_latitude correctly validates a valid latitude", {
  expect_true(validate_decimal_latitude(data.frame(decimalLatitude = 45.1234)))
})
test_that("validate_decimal_latitude identifies an invalid latitude
 (greater than 90)", {
  expect_false(validate_decimal_latitude(data.frame(decimalLatitude = 95.1234)))
})
test_that("validate_decimal_latitude identifies an invalid latitude
 (less than -90)", {
  expect_false(
    validate_decimal_latitude(data.frame(decimalLatitude = -95.1234))
  )
})
test_that("validate_decimal_latitude gives warning if no decimalLatitude
column is found", {
  expect_snapshot(local({
    validate_decimal_latitude(data.frame(latitude = -95.1234))
  }))
})

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  temporal validations
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

test_that("validate_event_date correctly prints to console", {
  df <- data.frame(
    eventDate = c(
      "1963-03-08T14:07-0600", # valid
      "2009-02-20T08:40Z", # valid
      "2018-08-29T15:19", # valid
      "1809-02-12", # valid
      "1906-06", # valid
      "1971", # valid
      "2007-03-01T13:00:00Z/2008-05-11T15:30:00Z", # valid
      "1900/1909", # valid
      "2007-11-13/15", # valid
      "2020-31-12", # invalid ISO 8601
      "2020-12-31T25:61", # invalid - hours 25
      "not a date", # invalid
      "2009-02-30" # invalid (non-existent date 30 feb)
    ),
    id = 1:13
  )
  expect_output(validate_event_date(df), regexp = "not a date")
})

test_that("validate_month_day ignores input that is not yyyy-mm-dd", {
  expect_true(validate_month_day("hello"))
})

test_that("validate_month_day correctly validates a valid date", {
  expect_true(validate_month_day("2022-01-01"))
})

test_that("validate_month_day correctly identifies an invalid date", {
  expect_false(validate_month_day("2022-13-01"))
})

test_that("validate_month_day identifies date with an invalid month", {
  expect_false(validate_month_day("2022-00-01"))
})

test_that("validate_month_day correctly identifies date with an invalid day", {
  expect_false(validate_month_day("2022-01-32"))
})
