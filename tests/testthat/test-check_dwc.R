test_that("check_basisOfRecord() works", {
  x <- tibble(basisOfRecord = "humanObservation")
  # no error
  x |>
    check_basisOfRecord() |>
    expect_no_error()
  result <- check_basisOfRecord(x)
  expect_equal(x, result)
  # with error
})

test_that("use_basisOfRecord() works", {
  x <- tibble(x = 1)
  # no error
  x |>
    use_basisOfRecord("humanObservation") |>
    expect_no_error()
  result <- use_basisOfRecord(x, "humanObservation")
  expect_s3_class(result, c("tbl_df", "tbl", "data.frame")) # dwc_df case?
  expect_equal(ncol(result), 2)
  expect_equal(colnames(result), c("x", "basisOfRecord"))
  # with error
  x |>
    use_basisOfRecord("something") |>
    expect_error(regexp = "basisOfRecord") 
  # ideally this would check for `Error in check_basisOfRecord()`;
  # but this isn't a message, so it doesn't work. May need to snapshot?
})