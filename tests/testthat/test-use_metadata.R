test_that("use_metadata() arguments work for R Markdown", {
  use_metadata_template("EXAMPLE.Rmd",
                        overwrite = TRUE,
                        quiet = TRUE)
  
  # fails when `input` doesn't exist
  use_metadata("something.file") |>
    expect_error()
  
  # builds `./data-publish` and `eml.xml`
  file.exists("data-publish") |>
    expect_false() 
  use_metadata("EXAMPLE.Rmd") |>
    expect_no_error()
  file.exists("data-publish") |>
    expect_true() 
  file.exists("data-publish/eml.xml") |>
    expect_true()
  unlink("data-publish", recursive = TRUE)
  
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
  unlink("data-publish", recursive = TRUE)
})

test_that("use_metadata() arguments work for Quarto Markdown", {
  use_metadata_template("EXAMPLE.Qmd",
                        overwrite = TRUE,
                        quiet = TRUE)

  # fails when `input` doesn't exist
  use_metadata("something.file") |>
    expect_error()

  # builds `./data-publish` and `eml.xml`
  file.exists("data-publish") |>
    expect_false()
  use_metadata("EXAMPLE.Qmd") |>
    expect_no_error()
  file.exists("data-publish") |>
    expect_true()
  file.exists("data-publish/eml.xml") |>
    expect_true()
  unlink("data-publish", recursive = TRUE)

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
  unlink("data-publish", recursive = TRUE)
})