# Set quarto path so that quarto can work correctly on CRAN
# To set this up locally prior pushing to `main`/GH Actions:
# - Find quarto location with `quarto::quarto_path()`
# - Assign that location to Env variable "quarto_path" with `usethis::edit_r_environ()`
# Sys.setenv(QUARTO_PATH=Sys.getenv("quarto_path")) 

# above lines commented out by MW 2025-07-03
# though fixing this up is a good idea if it allows testing of quarto on CI

test_that("use_metadata() fails when `file` is not set", {
  # set up
  temp_dir <- withr::local_tempdir()
  usethis::local_project(temp_dir, force = TRUE)
  use_metadata_template(quiet = TRUE) # add metadata.Rmd
  
  # tests
  use_metadata() |>
    expect_error()
  
  # clean up
  unlink(temp_dir)
})

test_that("use_metadata() works when `file` is set", {
  # set up
  temp_dir <- withr::local_tempdir()
  usethis::local_project(temp_dir, force = TRUE)
  use_metadata_template(quiet = TRUE) # add metadata.Rmd
  
  # tests
  use_metadata(file = "metadata.Rmd")
  expect_length(list.files("data-publish"), 1)
  expect_in("eml.xml", list.files("data-publish"))
  
  # clean up
  unlink("metadata.Rmd")
  unlink(temp_dir)
})

test_that("use_metadata() fails when input doesn't exist", {
  # set up
  temp_dir <- withr::local_tempdir()
  usethis::local_project(temp_dir, force = TRUE)
  use_metadata_template(quiet = TRUE) # add metadata.Rmd
  
  # tests
  use_metadata("something.file") |>
    expect_error()
  
  # clean up
  unlink("metadata.Rmd")
  unlink(temp_dir)
})

# test_that("use_metadata() saves xml file using provided filename", {
#   # set up
#   temp_dir <- withr::local_tempdir()
#   usethis::local_project(temp_dir, force = TRUE)
#   use_metadata_template(quiet = TRUE) # add metadata.Rmd
#   
#   # tests
#   use_metadata("metadata.Rmd", 
#                destination = "EXAMPLE.xml")
#   expect_true(
#     file.exists("data-publish/EXAMPLE.xml")
#     )
#   
#   # clean up
#   unlink("metadata.Rmd")
#   unlink(temp_dir)
# })

test_that("use_metadata() does not overwrite existing file by default", {
  # set up
  temp_dir <- withr::local_tempdir()
  usethis::local_project(temp_dir, force = TRUE)
  use_metadata_template(quiet = TRUE) # add metadata.Rmd
  
  # tests
  use_metadata(file = "metadata.Rmd")
  timestamp_1 <- file.info("data-publish/eml.xml")$ctime
  use_metadata(file = "metadata.Rmd") |>
    expect_message()
  timestamp_2 <- file.info("data-publish/eml.xml")$ctime
  expect_equal(timestamp_1, timestamp_2)
  
  # clean up
  unlink("metadata.Rmd")
  unlink(temp_dir)
})

test_that("use_metadata() overwrites file when overwrite = TRUE", {
  # set up
  temp_dir <- withr::local_tempdir()
  usethis::local_project(temp_dir, force = TRUE)
  use_metadata_template(quiet = TRUE) # add metadata.Rmd
  
  # tests
  use_metadata(file = "metadata.Rmd")
  timestamp_1 <- R.utils::lastModified("data-publish/eml.xml")
  Sys.sleep(2)
  expect_message(use_metadata(file = "metadata.Rmd", 
                              overwrite = TRUE))
  timestamp_2 <- R.utils::lastModified("data-publish/eml.xml")
  expect_true(timestamp_2 > timestamp_1)
  
  # clean up
  unlink("metadata.Rmd")
  unlink(temp_dir)
})

test_that("use_metadata() reads quarto doc", {
  skip_on_cran() # do not expect Quarto to be installed on CRAN
  skip_on_ci()   # cannot find Quarto on CI environment
  
  # set up
  temp_dir <- withr::local_tempdir()
  usethis::local_project(temp_dir, force = TRUE)
  use_metadata_template("metadata.qmd", quiet = TRUE) # add metadata.qmd
  
  # tests
  use_metadata(file = "metadata.qmd")
  expect_true(
    fs::file_exists("data-publish/eml.xml")
  )
  
  # clean up
  unlink("metadata.qmd")
  unlink(temp_dir)
})

