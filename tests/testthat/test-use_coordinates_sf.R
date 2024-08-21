
#---
# load small dataset of occurrence records
occs <- arrow::read_parquet(here::here("tests", "testthat", "testdata", "bandicoots.parquet"))

# set lat/lon to sf `geometry`
occs_clean <- occs |>
  tidyr::drop_na(decimalLatitude, decimalLongitude) |>
  sf::st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)
#---

test_that("use_coordinates_sf errors when missing .df", {
  expect_error(use_coordinates_sf(coords = geometry), 
               ".df is missing")
})

test_that("use_coordinates_sf errors when no dwc columns are named, or exist in the df", {
  df <- tibble(borp = c(149.125, 149.133))
  
  expect_error( # extra error when sf object isn't found
    expect_warning(suppressMessages(df |> use_coordinates_sf()),
                 "No Darwin Core terms detected")
    )
})

test_that("use_coordinates_sf returns tibble with updated dwc column names", {
  quiet_use_coordinates <- purrr::quietly(use_coordinates_sf)
  df <- occs_clean |>
    dplyr::select(recordID)
  # df2 <- tibble(borp = c(149.125, 149.133))
  
  df |> use_coordinates_sf()
  
  # TODO: Specifying coords argument causes incorrect term to be matched for `mutate()` and columns "checks"
  # find out why
  df |> use_coordinates_sf(coords = geometry) 
  
  result <- df |>
    quiet_use_coordinates_sf()
  
  expect_s3_class(result$result, c("tbl_df", "tbl", "data.frame"))
  expect_equal(colnames(result$result), c("decimalLongitude"))
})

test_that("use_coordinates_sf detects unnamed but existing dwc column names in df", {
  quiet_use_coordinates <- purrr::quietly(use_coordinates)
  df <- tibble(decimalLongitude = c(149.125, 149.133),
               decimalLatitude = c(-35.310, -35.273),
               col2 = 1:2)
  
  result <- df |>
    quiet_use_coordinates()
  
  expect_s3_class(result$result, c("tbl_df", "tbl", "data.frame"))
  expect_equal(colnames(result$result), c("decimalLongitude", "decimalLatitude", "col2"))
})

test_that("use_coordinates_sf has progress messages", {
  quiet_use_coordinates <- purrr::quietly(use_coordinates)
  df <- tibble(decimalLongitude = c(149.125, 149.133),
               decimalLatitude = c(-35.310, -35.273),
               col2 = 1:2)
  
  result <- df |> quiet_use_coordinates()
  
  expect_false(is.null(result$messages))
  
})