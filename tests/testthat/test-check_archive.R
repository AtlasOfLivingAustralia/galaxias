test_that("check_archive() works", {
  skip("tests not ready")
  skip_if_offline()
  
  galaxias_config(gbif = list(
    username = "atlasoflivingaustralia",
    email = "ala4r@ala.org.au",
    password = "galah-gbif-test-login"
  ))
  
  # check_archive(filename = "./tests/testthat/testdata/data/eml.xml")
})

test_that("retrieving a processed query works", {
  skip("tests_not_ready")
  result <- api_gbif_validator_status_get(
    key = "73253337-8fa2-466f-a78f-4c212c981fb7")
})