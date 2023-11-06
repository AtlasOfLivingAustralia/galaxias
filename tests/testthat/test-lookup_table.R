# Test that split_into_words works
# Test that word overlap detects matches correctly
# Easy example
data <- occurrence_data
col <- colnames(data)[3]
col2 <- "camelLanguage"
not <- "organism"
words1 <- split_into_words(col2)
words2 <- split_into_words(dwc_terms_archived$column_name[50])
length(intersect(words1, words2))

# this is a good string to test on because it gives 2 matches for a dwc term and
# 1 for another dwc term. so you can test filtering and ordering of matches etc.
col <- "infraspecific epithet"

test_mapping <- map_to_dwc(occurrence_data)
