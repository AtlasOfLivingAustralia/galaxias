library(tibble)
df <- tibble(
  latitude = c(-35.310, -35.273),
  longitude = c(149.125, 149.133),
  date = c("14-01-2023", "15-01-2023"),
  time = c("10:23", "11:25"),
  species = c("Callocephalon fimbriatum", "Eolophus roseicapilla"),
  n = c(2, 3))

test_that("use_datetime errors when missing .df", {
  expect_error(use_datetime(eventDate = eventDate), 
               ".df is missing")
})

test_that("use_datetime errors when no dwc columns are named, or exist in the df", {
  df <- tibble(col1 = "value")
  
  expect_error(df |> use_datetime(), 
               "No Darwin Core arguments supplied")
})

test_that("use_datetime returns tibble with updated dwc column names", {
  df <- tibble(user_col = dmy(c("14-01-2023", "15-01-2023")))
  
  result <- df |>
    use_datetime(eventDate = user_col)
  
  expect_s3_class(result, c("tbl_df", "tbl", "data.frame"))
  expect_match(colnames(result), c("eventDate"))
})

test_that("use_datetime detects unnamed but existing dwc column names in df", {
  df <- tibble(eventDate = dmy(c("14-01-2023", "15-01-2023")),
               col2 = 1:2)
  df2 <- tibble(eventDate = "borp",
                col2 = 1:2)
  
  result <- df |>
    use_datetime()
  
  expect_s3_class(result, c("tbl_df", "tbl", "data.frame"))
  expect_equal(colnames(result), c("eventDate", "col2"))
  expect_error(df2 |> use_datetime(), 
               "Unexpected value in eventDate")
})



