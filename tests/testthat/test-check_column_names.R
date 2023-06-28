test_that("check_percent_match_columns() gives information", {
  test_fields <- c("occurrenceID", "year", "something")
  expect_message({result <- check_percent_match_columns(test_fields)})
  expect_equal(test_fields, test_fields)
})

# tests needed for:
  # check_unique_identifier_columns()
  # check_mandatory_columns()
  # check_recommended_columns()