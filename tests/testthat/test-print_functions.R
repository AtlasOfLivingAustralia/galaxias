test_that("print.gbif_validator() runs", {
  print.gbif_validator(list(status = "downloading")) |>
    expect_message()
})

test_that("print.gbif_validator() colors work", {
  basic_message <- list("status" = "something") |>
    print_archive_status() |>
    capture_cli_messages()
  expect_equal(basic_message, 
               "Status: something\n")
  
  blue_message <- list("status" = "downloading") |>
    print_archive_status() |>
    capture_cli_messages()
  expect_equal(blue_message[1],
               "Status: downloading\n")
               # "Status: \033[34mdownloading\033[39m\n") # fails
  
  red_message <- list("status" = "failed") |>
    print_archive_status() |>
    capture_cli_messages()
  expect_equal(red_message[1],
               "Status: failed\n")
               # "Status: \033[31mfailed\033[39m\n")
  
  green_message <- list("status" = "finished") |>
    print_archive_status() |>
    capture_cli_messages()
  expect_equal(green_message[1],
               "Status: finished\n")
               # "Status: \033[32mfinished\033[39m\n")
})