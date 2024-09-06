
# still need to add other basic tests

test_that("use_abundance errors if individualCount = 0 & occurrenceStatus isn't in df", {
  df <- tibble(
    species = c("Callocephalon fimbriatum", "Eolophus roseicapilla"),
    # occurrenceStatus = c("present", "present"),
    individualCount = c(0, 2)
  )
  
  expect_error(
    suppressMessages(
      df |> use_abundance(individualCount = individualCount)
    ),
    "individualCount of 0 detected"
  )
})

test_that("use_abundance errors if `individualCount = 0` and `occurrenceStatus = 'absent'` don't match", {
  df <- tibble(
    species = c("Callocephalon fimbriatum", "Eolophus roseicapilla"),
    occurrenceStatus = c("present", "present"),
    individualCount = c(0, 2)
  )
  
  expect_error(
    suppressMessages(
      df |> use_abundance(individualCount = individualCount)
    ),
    "individualCount values do not match occurrenceStatus"
  )
})
