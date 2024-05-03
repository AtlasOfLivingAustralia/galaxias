test_that("decimalLatitude is properly defined", {
  skip_if(!any(colnames(occurrences) == "decimalLatitude"),
          message = "Column not supplied")
  field <- occurrences$decimalLatitude
  expect_true(class(field) %in% c("numeric", "integer"))
  expect_gte(field, -90)
  expect_lte(field, 90)
})

test_that("decimalLongitude is properly defined", {
  skip_if(!any(colnames(occurrences) == "decimalLongitude"),
          message = "Column not supplied")
  field <- occurrences$decimalLongitude
  expect_true(class(field) %in% c("numeric", "integer"))
  expect_gte(field, -180)
  expect_lte(field, 180)
})