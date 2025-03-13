test_that("galaxias_config() correctly stores information onload", {
  default_obj <- options("potions-pkg")
  expect_true(length(default_obj) > 0)
  # option to add further tests
})

test_that("galaxias_config object is correctly stored", {
  obj <- potions::pour(.pkg = "galaxias")
  expect_true(inherits(obj, "galaxias_config"))
  expect_equal(length(obj), 1)
  expect_equal(length(obj$gbif), 3)
  expect_setequal(names(obj$gbif), 
                  c("username", "email", "password"))
  expect_true(all(unlist(obj$gbif) == ""))
})

test_that("galaxias_config() accepts a correctly-formatted list", {
  credentials_obj <- list(
    username = "something",
    email = "my_email@email.com",
    password = "a-safe-password")
  galaxias_config(gbif = credentials_obj)
  result <- potions::pour(.pkg = "galaxias")
  expect_identical(credentials_obj, result$gbif)
})