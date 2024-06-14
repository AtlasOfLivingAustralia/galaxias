## generate example xml
# xml2::read_xml("https://collections.ala.org.au/ws/eml/dr368") |>
#   xml2::write_xml(file = "./tests/testthat/testdata/bionet_metadata_statement.xml")
## move above to input data script

test_that("`parse_metadata()` works", {
  x <- readLines("testdata/bionet_metadata.md")

  # error for no inputs
  parse_metadata() |> expect_error()

  # error for incorrect 'to' statement
  parse_metadata(x, to = "something") |> expect_error()

  # check returns a tibble by default
  y <- parse_metadata(x)
  expect_s3_class(y, c("tbl", "tbl_df", "data.frame"))
  expect_gte(nrow(y), 10)
  expect_equal(ncol(y), 4)
  expect_equal(colnames(y),
               c("level", "label", "attributes", "text"))

  # returns unaltered object if 'md' is given
  expect_equal(x, parse_metadata(x, to = "md"))

  # returns a list
  y <- parse_metadata(x, to = "list")
  expect_true(inherits(y, "list"))

  # returns an xml
  y <- parse_metadata(x, to = "xml")
  expect_s3_class(y, "xml_document")
})


test_that("parse_as_ functions work", {
  data_md <- readLines("testdata/bionet_metadata.md")
  data_xml <- xml2::read_xml("testdata/bionet_metadata.xml")
  
  # parse_as_md
  y <- parse_as_md(data_xml)
  expect_true(inherits(y, "character"))
  
  # parse_as_tibble
  x <- parse_as_tibble(data_md)
  expect_s3_class(x, c("tbl", "tbl_df", "data.frame"))
  y <- parse_as_tibble(data_xml)
  expect_s3_class(y, c("tbl", "tbl_df", "data.frame"))
  
  ## UP TO HERE
    # parse_md_to_tibble() is not identifying headers properly
    # AND is returning NULL rather than NA for $attributes
  expect_equal(nrow(x), nrow(y))
  expect_equal(x, y) # note: this is very stringent
  
  # parse_as_list
  x <- parse_as_list(data_md)
  expect_true(inherits(x, "list"))
  # Note that here we get some empty artifacts, e.g. $      : chr " \n        "
  # worth checking this
  y <- parse_as_tibble(data_xml)
  expect_true(inherits(x, "list"))
  expect_equal(length(x), length(y))
  expect_equal(x, y) # note: this is very stringent
  
  # parse_as_xml
})


test_that("parsing from md works", {
  x <- readLines("testdata/bionet_metadata.md")
  
  # parse_md_to_tibble()
  y <- parse_md_to_tibble(x)
  expect_s3_class(y, c("tbl", "tbl_df", "data.frame"))
  expect_gte(nrow(y), 10)
  expect_equal(colnames(y), 
               c("level", "label", "attributes", "text"))
  
  # parse_md_to_list
  y <- parse_md_to_list(x)
  expect_true(inherits(y, "list"))
  
  # parse_md_to_xml 
  y <- parse_md_to_xml(x)
  expect_s3_class(y, "xml_document")
})


test_that("parsing from tibble works", {
  x <- readLines("testdata/bionet_metadata.md") |>
    parse_md_to_tibble()
  
  # parse_tibble_to_md
  
  # parse_tibble_to_list
  y <- parse_tibble_to_list(x)
  expect_true(inherits(y, "list"))
  expect_equal(
    max(x$level),
    purrr::pluck_depth(y))
  
  # parse_tibble_to_xml  
})


test_that("parsing from a list works", {
  x <- readLines("testdata/bionet_metadata.md") |>
    parse_md_to_list()
  
  # parse_list_to_md
  
  # parse_list_to_tibble
  y <- parse_list_to_tibble(x)
  expect_s3_class(y, c("tbl", "tbl_df", "data.frame"))
  expect_gte(nrow(y), 10)
  expect_equal(colnames(y), 
               c("level", "label", "attributes", "text")) 
  
  # parse_list_to_xml  
})


# parse_xml_to_md
# parse_xml_to_tibble
# parse_xml_to_list