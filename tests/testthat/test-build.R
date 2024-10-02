test_that("build_ functions work correctly in sequence", {
  
  # set up example directory
  original_directory <- getwd()
  testdir <- "testdata" # necessary as it stores e.g. metadata.md
  glue("{original_directory}/{testdir}") |> setwd()
  dir.create("data")
 
  # add data
  # add events.csv
  tibble(eventID = 1, eventDate = "2024-01-01") |>
    write.csv(file = "data/events.csv",
              row.names = FALSE)
  # add occurrences.csv
  tibble(basisOfRecord = "humanObservation", individualCount = 1) |>
    write.csv(file = "data/occurrences.csv",
              row.names = FALSE)
  # expect_error(build_archive()) # no schema or metadata
  
  ## TEST 1: `build_schema()`
  # run function
  build_schema()
  expect_true(file.exists("data/meta.xml"))
  result <- readLines("data/meta.xml")
  expect_equal(length(result), 15) # correct number of entries
  expect_true(all(grepl("^\\s*<", result))) # all open with `<`
  # NOTE: still has problems with attributes containing `amp` instead of `&`
  # expect_error(build_archive()) # no metadata yet
  
  ## TEST 2: `build_metadata()`
  build_metadata("bionet_metadata.md")
  "data/eml.xml" |>
    file.exists() |>
    expect_true()
  result <- xml2::read_xml("data/eml.xml")
  result |>
    inherits("xml_document") |>
    expect_true()
  
  ## TEST 3: `build_archive()`
  build_archive()
  setwd(original_directory)
  archive <- glue("{testdir}.zip")
  archive |>
    file.exists() |>
    expect_true()
  
  contents <- utils::unzip(archive, list = TRUE)
  expect_equal(nrow(contents), 4)
  (c("eml.xml",
    "events.csv", 
    "meta.xml", 
    "occurrences.csv") %in% contents$Name) |>
    all() |>
    expect_true()

  # end
  unlink(glue("{testdir}/data"), recursive = TRUE)
  unlink("testdata.zip", recursive = TRUE)
  setwd(original_directory)
})