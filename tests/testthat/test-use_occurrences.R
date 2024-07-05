library(tibble)
df <- tibble(
  latitude = c(-35.310, -35.273),
  longitude = c(149.125, 149.133),
  date = c("14-01-2023", "15-01-2023"),
  time = c("10:23", "11:25"),
  species = c("Callocephalon fimbriatum", "Eolophus roseicapilla"),
  n = c(2, 3))

test_that("use_occurrences errors when missing .df", {
  expect_error(use_occurrences(basisOfRecord = basisOfRecord), 
               ".df is missing")
})

test_that("use_occurrences errors when no dwc columns are named, or exist in the df", {
  df <- tibble(basisOfRecord = "humanObservation")
  
  expect_error(df |> use_occurrences(), 
               "No Darwin Core arguments supplied")
})

test_that("use_occurrences returns tibble with updated dwc column names", {
  df <- tibble(user_col = "humanObservation")
  
  result <- df |>
    use_occurrences(basisOfRecord = user_col)
  
  expect_s3_class(result, c("tbl_df", "tbl", "data.frame"))
  expect_match(colnames(result), c("basisOfRecord"))
})

test_that("use_occurrences detects existing dwc column names in df", {
  df <- tibble(basisOfRecord = "humanObservation",
               col2 = 1:2)
  
  result <- df |>
    use_occurrences()
  
  expect_s3_class(result, c("tbl_df", "tbl", "data.frame"))
  expect_match(colnames(result), c("basisOfRecord"))
})

# test_that("use_occurrences runs correct checks for each column", {
#   df <- tibble(basisOfRecord = "humanObservation",
#                col2 = 1:2)
#   
#   result <- df |>
#     use_occurrences()
#   
#   expect_s3_class(result, c("tbl_df", "tbl", "data.frame"))
#   expect_match(colnames(result), c("basisOfRecord"))
# })

# specific error messages checks
# 