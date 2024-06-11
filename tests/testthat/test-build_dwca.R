test_that("`create_bd_project()` correctly generates a project", {
  # build a skeleton project
  # testdir <- tempdir() # this is per-session; do not `unlink()`
  # proj_name <- glue("{testdir}/testproject") # this is the test package; `unlink()` after testing
  
  # for testing
  proj_name <- "/Users/wes186/Documents/Work/Development/AtlasOfLivingAustralia/galaxias-test" 
  
  # ok from here
  create_bd_project(proj_name, open = FALSE)
  unlink(glue("{proj_name}/data-raw"), recursive = TRUE) # remove unneeded empty content
  browser() # use when building or checking this test file
  # add populated content in correct locations
  file.copy(from = "testdata/occurrence_exemplar.csv",
            to = glue("{proj_name}/data/occurrences.csv"))
  file.copy(from = "testdata/example_metadata.md",
            to = glue("{proj_name}/metadata.md"),
            overwrite = TRUE)
  
  # list.files(proj_name, recursive = TRUE) # check contents are as expected
  
 
  
  
  # tidy up
  unlink(proj_name, recursive = TRUE)
})