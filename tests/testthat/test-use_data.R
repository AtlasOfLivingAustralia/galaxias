
# test_that("use_data() works with no arguments", {
#   # set up
#   current_wd <- here::here()
#   temp_dir <- withr::local_tempdir()
#   usethis::local_project(temp_dir, force = TRUE)
#   df <- tibble::tibble(
#     decimalLatitude = c(44.4, 44.4)
#   ) |>
#     dplyr::mutate(
#       occurrenceID = random_id()
#     )
#   
#   #tests
#   df |> use_data(quiet = TRUE)
#   expect_length(list.files("data-publish"), 1)
#   expect_in("occurrences.csv", list.files("data-publish"))
#   
#   # clean up
#   unlink(temp_dir)
# })
