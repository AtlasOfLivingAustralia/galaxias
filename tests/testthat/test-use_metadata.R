test_that("use_metadata() works with no arguments", {
  
  current_wd <- here::here()
  temp_dir <- withr::local_tempdir()
  usethis::local_project(temp_dir, force = TRUE)
  dir.create("data-publish")
  write(readLines(paste0(current_wd, "/metadata.Rmd")), "metadata.Rmd")

  use_metadata()
  expect_length(list.files("data-publish"), 1)
  expect_in("eml.xml", list.files("data-publish"))
  
})
