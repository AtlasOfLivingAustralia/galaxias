# test work overlap score function
test_that("word_overlap detects number of matches correctly", {
  words1 <- c("species", "date", "random")
  words2 <- c("species", "date", "latitude")
  result <- word_overlap_score(words1, words2)
  expect_equal(result, 2)
})

# test string splitting with split_into_words
test_that("split_into_words splits camel case correctly", {
  string <- "camelGang"
  result <- split_into_words(string)
  expect_equal(result, c("camel", "gang"))
})
test_that("split_into_words splits separators", {
  string <- "column-title/chaos_theory"
  result <- split_into_words(string)
  expect_equal(result, c("column", "title", "chaos", "theory"))
})
