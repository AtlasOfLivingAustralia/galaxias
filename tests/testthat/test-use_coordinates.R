
test_that("use_coordinates errors when missing .df", {
  expect_error(use_coordinates(decimalLongitude = c(149.125, 149.133)), 
               ".df is missing")
})

test_that("use_coordinates errors when no dwc columns are named, or exist in the df", {
  df <- tibble(borp = c(149.125, 149.133))
  
  expect_warning(suppressMessages(use_scientific_name(df)),
                 "No Darwin Core terms detected")
})

test_that("use_coordinates returns tibble with updated dwc column names", {
  quiet_use_coordinates <- purrr::quietly(use_coordinates)
  df <- tibble(user_col = c(149.125, 149.133))
  
  result <- df |>
    quiet_use_coordinates(decimalLongitude = user_col)
  
  expect_s3_class(result$result, c("tbl_df", "tbl", "data.frame"))
  expect_equal(colnames(result$result), c("decimalLongitude"))
})

test_that("use_coordinates detects unnamed but existing dwc column names in df", {
  quiet_use_coordinates <- purrr::quietly(use_coordinates)
  df <- tibble(decimalLongitude = c(149.125, 149.133),
               decimalLatitude = c(-35.310, -35.273),
               col2 = 1:2)
  
  result <- df |>
    quiet_use_coordinates()
  
  expect_s3_class(result$result, c("tbl_df", "tbl", "data.frame"))
  expect_equal(colnames(result$result), c("decimalLongitude", "decimalLatitude", "col2"))
})

test_that("use_coordinates has progress messages", {
  quiet_use_coordinates <- purrr::quietly(use_coordinates)
  df <- tibble(decimalLongitude = c(149.125, 149.133),
               decimalLatitude = c(-35.310, -35.273),
               col2 = 1:2)
  
  result <- df |> quiet_use_coordinates()
  
  expect_false(is.null(result$messages))
  
})


test_that("use_coordinates checks decimalLongitude format", {
  quiet_use_coordinates <- purrr::quietly(use_coordinates)
  df <- tibble(decimalLongitude = c(149.125, 149.133),
               col_string = c("string", "string"),
               col_bignumber = c(190, 149.133))
  
  result <- df |> quiet_use_coordinates()
  
  expect_s3_class(result$result, c("tbl_df", "tbl", "data.frame"))
  expect_equal(colnames(result$result), c("decimalLongitude", "col_string", "col_bignumber"))
  
  expect_error(suppressMessages(
    df |> use_coordinates(decimalLongitude = col_string)
  ),
  "decimalLongitude must be a numeric vector, not character"
  )
  expect_error(suppressMessages(
    df |> use_coordinates(decimalLongitude = col_bignumber)
  ),
  "Value is outside of expected range in decimalLongitude"
  )
})

test_that("use_coordinates checks decimalLatitude format", {
  quiet_use_coordinates <- purrr::quietly(use_coordinates)
  df <- tibble(decimalLatitude = c(-35.310, -35.273),
               col_string = c("string", "string"),
               col_bignumber = c(97, -35.273))
  
  result <- df |> quiet_use_coordinates()
  
  expect_s3_class(result$result, c("tbl_df", "tbl", "data.frame"))
  expect_equal(colnames(result$result), c("decimalLatitude", "col_string", "col_bignumber"))
  
  expect_error(suppressMessages(
    df |> use_coordinates(decimalLatitude = col_string)
  ),
  "decimalLatitude must be a numeric vector, not character"
  )
  expect_error(suppressMessages(
    df |> use_coordinates(decimalLatitude = col_bignumber)
  ),
  "Value is outside of expected range in decimalLatitude"
  )
})

test_that("use_coordinates checks geodeticDatum for valid CRS", {
  quiet_use_coordinates <- purrr::quietly(use_coordinates)
  df <- tibble(geodeticDatum = c("WGS84", "WGS84"),
               col_number = c(97, -35.273),
               col_not_real = c("WGS84", "WGS8d"))
  
  result <- df |> quiet_use_coordinates()
  
  expect_s3_class(result$result, c("tbl_df", "tbl", "data.frame"))
  expect_equal(colnames(result$result), c("geodeticDatum", "col_number", "col_not_real"))
  
  expect_error(suppressMessages(
    df |> use_coordinates(geodeticDatum = col_not_real)
  ),
  "geodeticDatum contains invalid Coordinate Reference System"
  )
  expect_warning(suppressMessages(
    df |> use_coordinates(geodeticDatum = col_number)
  ),
  "geodeticDatum contains unrecognised Coordinate"
  )
})
