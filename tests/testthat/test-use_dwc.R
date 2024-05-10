test_that("use_dwc() changes class, but doesn't affect tibble printing", {
  library(tibble)
  df <- tibble(
    latitude = c(-35.310, -35.273),
    longitude = c(149.125, 149.133),
    date = c("14-01-2023", "15-01-2023"),
    time = c("10:23", "11:25"),
    species = c("Callocephalon fimbriatum", "Eolophus roseicapilla"),
    n = c(2, 3))
  df |>
    # use_basisOfRecord("Something") # errors
    # use_basisOfRecord("humanObservation") # ok
    use_dwc() |>
    rename(decimalLatitude = latitude) |>
    mutate(basisOfRecord = "something") # should use `inform`; but not added yet
    # check_dwc() # no errors, as all fields are correctly specified
  # x # prints as a tibble
  expect_s3_class(x, "dwc")
})


test_that("use_dwc() defers reporting until end of a pipe", {
  library(tibble)
  df <- tibble(
    latitude = c(-35.310, -35.273),
    longitude = c(149.125, 149.133),
    date = c("14-01-2023", "15-01-2023"),
    time = c("10:23", "11:25"),
    species = c("Callocephalon fimbriatum", "Eolophus roseicapilla"),
    n = c(2, 3))
  df |>
    use_dwc() |>
    rename(decimalLatitude = latitude) |>
    mutate(basisOfRecord = "humanObservation")
})