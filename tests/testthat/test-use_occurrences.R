
test_that("use_occurrences errors when missing .df", {
  expect_error(use_occurrences(basisOfRecord = basisOfRecord), 
               ".df is missing")
})

test_that("use_occurrences errors when no dwc columns are named, or exist in the df", {
  df <- tibble(borp = "humanObservation")
  
  expect_warning(suppressMessages(use_occurrences(df)),
                 "No Darwin Core terms detected")
})

test_that("use_occurrences returns tibble with updated dwc column names", {
  quiet_use_occurrences <- purrr::quietly(use_occurrences)
  df <- tibble(user_col = "humanObservation")
  
  result <- df |>
    quiet_use_occurrences(basisOfRecord = user_col)
  
  expect_s3_class(result$result, c("tbl_df", "tbl", "data.frame"))
  expect_match(colnames(result$result), c("basisOfRecord"))
})

test_that("use_occurrences detects unnamed but existing dwc column names in df", {
  quiet_use_occurrences <- purrr::quietly(use_occurrences)
  df <- tibble(basisOfRecord = "humanObservation",
               col2 = 1:2)
  df2 <- tibble(basisOfRecord = "borp",
                col2 = 1:2)
  
  result <- df |>
    quiet_use_occurrences()
  
  expect_s3_class(result$result, c("tbl_df", "tbl", "data.frame"))
  expect_equal(colnames(result$result), c("basisOfRecord", "col2"))
  expect_error(
    suppressMessages(
      df2 |> use_occurrences()
      ), 
    "Unexpected value in basisOfRecord")
})

test_that("use_occurrences has progress messages", {
  quiet_use_occurrences <- purrr::quietly(use_occurrences)
  df <- tibble(basisOfRecord = "humanObservation",
               col2 = 1:2)
  
  result <- df |> quiet_use_occurrences()
  
  expect_false(is.null(result$messages))
  
})

test_that("use_occurrences handles `use_id_random()`", {
  quiet_use_occurrences <- purrr::quietly(use_occurrences)
  df <- tibble(basisOfRecord = "humanObservation",
               col2 = 1:2)

  result <- df |>
    quiet_use_occurrences(occurrenceID = use_id_random())
  
  expect_s3_class(result$result, c("tbl_df", "tbl", "data.frame"))
  expect_equal(colnames(result$result), c("basisOfRecord", "col2", "occurrenceID"))
  expect_type(result$result$occurrenceID, "character")
  expect_equal(nchar(result$result$occurrenceID), c(36, 36))
})

test_that("use_id_random() generates unique UUID", {
  quiet_use_occurrences <- purrr::quietly(use_occurrences)
  df <- tibble(
    basisOfRecord = "humanObservation",
    col2 = 1:2
  )

  result <- df |>
    quiet_use_occurrences(occurrenceID = use_id_random())
  
  # if any aren't UUIDs, they will return NA
  uuid_check <- result |> 
    purrr::pluck("result") |>
    select(occurrenceID) |>
    purrr::map_dfr(uuid::as.UUID)

  expect_type(result$result$occurrenceID, "character")
  expect_equal(nchar(result$result$occurrenceID), c(36, 36))
  expect_true(all(!is.na(uuid_check)))
  expect_equal(length(unique(result$result$occurrenceID)), nrow(result$result))
})

test_that("use_occurrences errors when UUID is already present in df", {
  df <- tibble(basisOfRecord = "humanObservation",
               id_col = uuid::UUIDgenerate())
  
  expect_error(suppressMessages(
    use_occurrences(df, occurrenceID = use_id_random())),
    "Column id_col contains UUID values")
})

test_that("use_occurrences only accepts valid values for basisOfRecord", {
  valid_values <- c("humanObservation", "machineObservation", "livingSpecimen",
                       "preservedSpecimen", "fossilSpecimen", "materialCitation")
  
  df_right <- tibble(basisOfRecord = valid_values)
  df_wrong <- tibble(basisOfRecord = c(valid_values, "blop"))
  
  expect_no_error(suppressMessages(
    df_right |> use_occurrences(basisOfRecord = basisOfRecord)
    ))
  expect_error(suppressMessages(
    df_wrong |> use_occurrences(basisOfRecord = basisOfRecord)),
    "Unexpected value in basisOfRecord"
    )
  expect_error(suppressMessages(
    df_wrong |> use_occurrences(basisOfRecord = 3)),
    "basisOfRecord must be a character vector, not numeric"
    )
})

