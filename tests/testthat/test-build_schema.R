test_that("build_schema() creates a valid xml in `/data`", {
  # set up example directory
  # place /data in temp
  # testdir <- tempdir() # this is per-session; do not `unlink()`
  testdir <- "testdata_temp"
  dir.create(testdir)
  glue("{testdir}/data") |> dir.create()
  # add events.csv
  tibble(eventID = 1, eventDate = "2024-01-01") |>
    write.csv(file = glue("{testdir}/data/events.csv"),
              row.names = FALSE)
  # add occurrences.csv
  tibble(basisOfRecord = "humanObservation", individualCount = 1) |>
    write.csv(file = glue("{testdir}/data/occurrences.csv"),
              row.names = FALSE)
  browser()
  # run function
  build_schema(directory = testdir)
  # find file
  glue("{testdir}/data/meta.xml") |>
    file.exists() |>
    expect_true()
  
  # scan() then use grepl() to detect specific strings
  # NOTE: there is currently a bug here: no `text` or `attributes` are being 
  # saved to `meta.xml`
  
  unlink(testdir, recursive = TRUE)
    
})