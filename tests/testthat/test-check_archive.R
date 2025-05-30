test_that("check_archive() works", {
  skip_if_offline()
  
  # set up directory
  current_wd <- here::here()
  temp_dir <- withr::local_tempdir()
  usethis::local_project(temp_dir, force = TRUE)
  
  # set gbif credentials
  galaxias_config(
    gbif = list(
      username = "atlasoflivingaustralia",
      email = "ala4r@ala.org.au",
      password = "galah-gbif-test-login"
    ))
  
  # build galaxias-specific content
  usethis::use_directory("data-publish")
  use_metadata_template(quiet = TRUE)
  use_metadata("metadata.Rmd", quiet = TRUE)
  tibble::tibble(
    occurrenceID = random_id(),
    decimalLatitude = c(44.4, 44.4)) |>
    write.csv("data-publish/occurrences.csv")
  use_schema(quiet = TRUE)
  build_archive(quiet = TRUE)
  
  # tests
  
  
  # clean up
  unlink("../dwc-archive.zip")
  unlink("metadata.Rmd")
  unlink("data-publish")
  unlink(temp_dir)
  
  
  result <- check_archive()
  # check printing etc
  
  ## Sys.sleep(60) 
  # this is a poor solution. Need to have repeated checking with cooldown
  # can get code from galah for that
})

test_that("retrieving a processed query works", {
  skip_if_offline()
  skip("Tests not ready")
  result <- get_report("73253337-8fa2-466f-a78f-4c212c981fb7")
})