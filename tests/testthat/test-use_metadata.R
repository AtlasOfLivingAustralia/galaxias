test_that("use_metadata() arguments work for R Markdown", {
  use_metadata_template("EXAMPLE.Rmd",
                        overwrite = TRUE,
                        quiet = TRUE)
  
  # fails when `input` doesn't exist
  use_metadata("something.file") |>
    expect_error()

  # first check no file already present
  file.exists("data-publish") |> # no file at first
    expect_false()
  
  # now we have a problem, because `usethis` only puts information in the project directory
  root_path <- usethis::proj_path() |> 
    as.character() # where `usethis` places folders
  current_path <- getwd() # where `testthat` is working
  # these are likely to be different at all times, but hard to be sure
  # where they *are* different, we need to add `data-publish` here
  if(root_path != current_path){
    dir.create("data-publish")
    use_metadata("EXAMPLE.Rmd") |>
      expect_no_error()
    root_directory <- file.path(root_path, "data-publish")
    root_directory |>
      file.exists() |>
      expect_true()
  }else{
    # this is how tests should actually work
    use_metadata("EXAMPLE.Rmd") |>
      expect_no_error()
    file.exists("data-publish") |>
      expect_true()
  }
  
  # check file exists  
  file.exists("data-publish/eml.xml") |>
    expect_true()
  
  # setting a file name generates a file with that name, which is valid EML
  use_metadata("EXAMPLE.Rmd", 
               destination = "EXAMPLE.xml")
  file.exists("data-publish/EXAMPLE.xml") |>
    expect_true()

  # where file already exists (as created in prev test), default is to not overwrite 
  timestamp_1 <- file.info("data-publish/EXAMPLE.xml")$ctime
    use_metadata("EXAMPLE.Rmd",
                 destination = "EXAMPLE.xml") |>
    expect_message()
  timestamp_2 <- file.info("data-publish/EXAMPLE.xml")$ctime
  expect_equal(timestamp_1, timestamp_2)
  
  # `overwrite` works, when requested
  timestamp_1 <- file.info("data-publish/EXAMPLE.xml")$ctime
  use_metadata("EXAMPLE.Rmd", 
               destination = "EXAMPLE.xml",
               overwrite = TRUE) |>
    expect_message()
  timestamp_2 <- file.info("data-publish/EXAMPLE.xml")$ctime
  expect_true(timestamp_2 > timestamp_1)
  
  # check reimporting works
  x <- xml2::read_xml("data-publish/EXAMPLE.xml")
  inherits(x, c("xml_document", "xml_node")) |>
    expect_true()
  # ditto but to tibble
  delma::read_eml("data-publish/EXAMPLE.xml") |>
    expect_no_error()
  
  # clean up
  unlink("EXAMPLE.Rmd")
  unlink(root_directory)
  unlink("data-publish", recursive = TRUE)
})

test_that("use_metadata() arguments work for Quarto Markdown", {
  use_metadata_template("EXAMPLE.Qmd",
                        overwrite = TRUE,
                        quiet = TRUE)

  # fails when `input` doesn't exist
  use_metadata("something.file") |>
    expect_error()

  # first check no file already present
  file.exists("data-publish") |> # no file at first
    expect_false()
  
  # now we have a problem, because `usethis` only puts information in the project directory
  root_path <- usethis::proj_path() |> 
    as.character() # where `usethis` places folders
  current_path <- getwd() # where `testthat` is working
  # these are likely to be different at all times, but hard to be sure
  # where they *are* different, we need to add `data-publish` here
  if(root_path != current_path){
    dir.create("data-publish")
    use_metadata("EXAMPLE.Qmd") |>
      expect_no_error()
    root_directory <- file.path(root_path, "data-publish")
    root_directory |>
      file.exists() |>
      expect_true()
  }else{
    # this is how tests should actually work
    use_metadata("EXAMPLE.Qmd") |>
      expect_no_error()
    file.exists("data-publish") |>
      expect_true()
  }

  # check file exists  
  file.exists("data-publish/eml.xml") |>
    expect_true()

  # setting a file name generates a file with that name, which is valid EML
  use_metadata("EXAMPLE.Qmd",
               destination = "EXAMPLE.xml")
  file.exists("data-publish/EXAMPLE.xml") |>
    expect_true()

  # where file already exists (as created in prev test), default is to not overwrite
  timestamp_1 <- file.info("data-publish/EXAMPLE.xml")$ctime
  use_metadata("EXAMPLE.Qmd",
               destination = "EXAMPLE.xml") |>
    expect_message()
  timestamp_2 <- file.info("data-publish/EXAMPLE.xml")$ctime
  expect_equal(timestamp_1, timestamp_2)

  # `overwrite` works, when requested
  timestamp_1 <- file.info("data-publish/EXAMPLE.xml")$ctime
  use_metadata("EXAMPLE.Qmd",
               destination = "EXAMPLE.xml",
               overwrite = TRUE) |>
    expect_message()
  timestamp_2 <- file.info("data-publish/EXAMPLE.xml")$ctime
  expect_true(timestamp_2 > timestamp_1)

  # check reimporting works
  x <- xml2::read_xml("data-publish/EXAMPLE.xml")
  inherits(x, c("xml_document", "xml_node")) |>
    expect_true()
  # ditto but to tibble
  delma::read_eml("data-publish/EXAMPLE.xml") |>
    expect_no_error()

  # clean up
  unlink("EXAMPLE.Qmd")
  unlink(root_directory)
  unlink("data-publish", recursive = TRUE)
})

# adding this at Dax's request
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