
test_that("use_scientific_name errors when missing .df", {
  expect_error(use_scientific_name(scientificName = scientificName), 
               ".df is missing")
})

test_that("use_scientific_name errors when no dwc columns are named, or exist in the df", {
  df <- tibble(borp = c("Callocephalon fimbriatum", "Eolophus roseicapilla"))
  
  expect_warning(suppressMessages(use_scientific_name(df)),
                 "No Darwin Core terms detected")
})

test_that("use_scientific_name returns tibble with updated dwc column names", {
  quiet_use_scientific_name <- purrr::quietly(use_scientific_name)
  df <- tibble(user_col = c("Callocephalon fimbriatum", "Eolophus roseicapilla"))
  
  result <- df |>
    quiet_use_scientific_name(scientificName = user_col)
  
  expect_s3_class(result$result, c("tbl_df", "tbl", "data.frame"))
  expect_equal(colnames(result$result), c("scientificName"))
})

test_that("use_scientific_name detects unnamed but existing dwc column names in df", {
  quiet_use_scientific_name <- purrr::quietly(use_scientific_name)
  df <- tibble(scientificName = c("Callocephalon fimbriatum", "Eolophus roseicapilla"),
               scientificNameAuthorship = c("Fred", "Mary"),
               col2 = 1:2)
  
  result <- df |>
    quiet_use_scientific_name()
  
  expect_s3_class(result$result, c("tbl_df", "tbl", "data.frame"))
  expect_equal(colnames(result$result), c("scientificName", "scientificNameAuthorship", "col2"))
})

test_that("use_scientific_name has progress messages", {
  quiet_use_scientific_name <- purrr::quietly(use_scientific_name)
  df <- tibble(scientificName = c("Callocephalon fimbriatum", "Eolophus roseicapilla"),
               col2 = 1:2)
  
  result <- df |> quiet_use_scientific_name()
  
  expect_false(is.null(result$messages))
  
})

test_that("use_scientific_name checks scientificName format", {
  quiet_use_scientific_name <- purrr::quietly(use_scientific_name)
  df <- tibble(scientificName = c("Callocephalon fimbriatum", "Eolophus roseicapilla"),
               col2 = 1:2)
  
  result <- df |> quiet_use_scientific_name()
  
  expect_s3_class(result$result, c("tbl_df", "tbl", "data.frame"))
  expect_equal(colnames(result$result), c("scientificName", "col2"))
  
  expect_error(suppressMessages(
    df |> use_scientific_name(scientificName = col2)
  ),
  "scientificName must be a character vector, not integer"
  )
})

test_that("use_scientific_name checks scientificNameRank format", {
  quiet_use_scientific_name <- purrr::quietly(use_scientific_name)
  df <- tibble(scientificNameRank = c("family", "species"),
               col2 = 1:2)
  
  result <- df |> quiet_use_scientific_name()
  
  expect_s3_class(result$result, c("tbl_df", "tbl", "data.frame"))
  expect_equal(colnames(result$result), c("scientificNameRank", "col2"))
  
  expect_error(suppressMessages(
    df |> use_scientific_name(scientificNameRank = col2)
  ),
  "scientificNameRank must be a character vector, not integer"
  )
})

test_that("use_scientific_name checks scientificNameAuthorship format", {
  quiet_use_scientific_name <- purrr::quietly(use_scientific_name)
  df <- tibble(scientificNameAuthorship = c("(Györfi, 1952)", "R. A. Graham"),
               col2 = 1:2)
  
  result <- df |> quiet_use_scientific_name()
  
  expect_s3_class(result$result, c("tbl_df", "tbl", "data.frame"))
  expect_equal(colnames(result$result), c("scientificNameAuthorship", "col2"))
  
  expect_error(suppressMessages(
    df |> use_scientific_name(scientificNameAuthorship = col2)
  ),
  "scientificNameAuthorship must be a character vector, not integer"
  )
})
