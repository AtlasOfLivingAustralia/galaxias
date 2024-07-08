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

test_that("use_occurrences handles `use_id_random()`", {
  df <- tibble(basisOfRecord = "humanObservation",
               col2 = 1:2)

  result <- df |>
    use_occurrences(occurrenceID = use_id_random())
  
  expect_s3_class(result, c("tbl_df", "tbl", "data.frame"))
  expect_match(colnames(result), c("basisOfRecord", "col2", "occurrenceID"))
  expect_s3_class(result$occurrenceID, "character")
  expect_match(nchar(result$occurrenceID), c(36, 36))
})

test_that("use_id_random() generates UUID", {
  test_that("use_occurrences handles `use_id_random()`", {
    df <- tibble(basisOfRecord = "humanObservation",
                 col2 = 1:2)
    
    result <- df |>
      use_occurrences(occurrenceID = use_id_random())
    
    # if any aren't UUIDs, they will return NA
    uuid_check <- result |>
      select(occurrenceID) |>
      purrr::map_dfr(uuid::as.UUID)
    
    expect_s3_class(result$occurrenceID, "character")
    expect_match(nchar(result$occurrenceID), c(36, 36))
    expect_true(all(!is.na(uuid_check)))
    expect_equal(length(unique(result$occurrenceID)), length(result))
  })
})

test_that("use_occurrences errors when UUID is already present in df", {
  df <- tibble(basisOfRecord = "humanObservation",
               id_col = uuid::UUIDgenerate())
  
  expect_error(use_occurrences(df, occurrenceID = use_id_random()),
               "Column id_col contains UUID values")
})

test_that("use_occurrences only accepts valid values", {
  valid_values <- c("humanObservation", "machineObservation", "livingSpecimen",
                       "preservedSpecimen", "fossilSpecimen", "materialCitation")
  
  df_right <- tibble(basisOfRecord = valid_values)
  df_wrong <- tibble(basisOfRecord = c(valid_values, "blop"))
  
  expect_no_error(df_right |> use_occurrences(basisOfRecord = basisOfRecord))
  expect_error(df_wrong |> use_occurrences(basisOfRecord = basisOfRecord),
               "Unexpected value in basisOfRecord")
  expect_error(df_wrong |> use_occurrences(basisOfRecord = 3),
               "Unexpected value in basisOfRecord")
})

# specific error messages checks
# 