test_that("decimalLatitude is properly defined", {
  skip_if(!any(colnames(.dwca$occurrences) == "decimalLatitude"))
  field <- .dwca$occurrences$decimalLatitude
  expect_true(class(field) %in% c("numeric", "integer"))
  expect_gte(field, 0)
  expect_lte(field, 90)
})