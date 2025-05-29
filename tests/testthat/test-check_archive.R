test_that("check_archive() works", {
  skip_if_offline()
  skip("Tests not ready")
  
  # use default credentials and localled cached zip file
  galaxias_config(
    archive = "testdata/simple_dwca.zip",
    gbif = list(
      username = "atlasoflivingaustralia",
      email = "ala4r@ala.org.au",
      password = "galah-gbif-test-login"
    ))
  
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