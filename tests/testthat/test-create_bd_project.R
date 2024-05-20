test_that("`usethis` functions generate a project", {
  testdir <- tempdir() # this is per-session; do not `unlink()`
  proj_name <- paste0(testdir, "/testproject") # this is the test package
  
  # test default files are added correctly
  create_bd_project(proj_name, open = FALSE)
  file_list <- list.files(proj_name, 
                          pattern = "[[:alpha:]]+", # only file names containing letters
                          all.files = TRUE)
  expect_contains(file_list,
                  c("data",
                    "data-raw",
                    "README.md",
                    "metadata.md",
                    "testproject.Rproj"))
  
  ## add schema
  
  # tidy up
  unlink(proj_name, recursive = TRUE)
})