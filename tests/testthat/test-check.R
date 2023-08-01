# Test check_ functions
# Test for successfully detect missing columns
# Test for null response if no missing columns

# Read in an example dataset
occurrence_data <- read.csv(file = "data-raw/westerband_2022_wdate.csv")

# Test that check_required_fields returns a message because required fields are
# missing
test_that("check_required_fields returns a message because required fields are missing", {
  expect_message(check_required_fields(occurrence_data), "Missing required columns.")
})
head(occurrence_data)

# are they missing or are they just incorrectly named?

# Test that check_required_fields returns ??? if all required fields are
# present.
