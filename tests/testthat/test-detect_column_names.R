test_that("detect_column_names() returns a happy message when all columns are matched", {
  library(tibble)
  library(lubridate)
  library(dplyr)
  data <- tibble(basisOfRecord = c("human observation", "human observation"),
                 eventDate = c(as_date(ymd("2020-01-01")), as_date(ymd("2020-01-01"))),
                 scientificName = c("perameles", "perameles"),
                 occurrenceID = c(1:2))
  expect_message({result <- detect_column_names(data)})
  expect_true(all(colnames(result) == colnames(data)))
})

## In progress
# # works with happy message and changes names
# dat_snake <- tibble(basis_of_record = c("human observation", "human observation"),
#                     event_date = c(as_date(ymd("2020-01-01")), as_date(ymd("2020-01-01"))),
#                     scientific_name = c("perameles", "perameles"),
#                     occurrence_ID = c(1:2))
# 
# detect_column_names(dat_snake)
# 
# # works with sad messages and changes names of matched columns
# dat_wrong <-tibble(basisOfRecord = c("human observation", "human observation"),
#                    eventDate = c(as_date(ymd("2020-01-01")), as_date(ymd("2020-01-01"))),
#                    scientific_name = c("perameles", "perameles"),
#                    occurrenceID = c(1:2),
#                    bork_wiggle = 1:2,
#                    forkPotato_unDF_ID = 1:2)
# 
# detect_column_names(dat_wrong)