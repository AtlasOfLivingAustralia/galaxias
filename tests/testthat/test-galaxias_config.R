test_that("potions::pour creates the correct object", {
  default_obj <- options("potions-pkg")
  expect_true(length(default_obj) > 0)
})

test_that("galaxias_config() creates default object onload", {
  cached_obj <- potions::pour(.pkg = "galaxias")
  comparison_obj <- galaxias_default_config(
    directory = "data-publish",
    archive = glue::glue("{here::here()}.zip"))
  expect_identical(cached_obj, comparison_obj)  
})

test_that("galaxias_config() returns an object if all args are missing", {
  cached_obj <- galaxias_config()
  comparison_obj <- galaxias_default_config(
    directory = "data-publish",
    archive = glue::glue("{here::here()}.zip"))
  expect_identical(cached_obj, comparison_obj) 
})

test_that("galaxias_config() rejects non-character directories", {
  expect_error(galaxias_config(directory = 100L))
})

test_that("galaxias_config() accepts non-default directories, but DOES NOT build them", {
  dirname <- "MY-NEW-DIRECTORY"
  galaxias_config(directory = dirname)
  result <- galaxias_config()$directory
  expect_equal(dirname, result)
  expect_false(file.exists(result))
  # restore defaults
  galaxias_config(directory = "data-publish",
                  archive = glue::glue("{here::here()}.zip"))
})

test_that("galaxias_config() rejects non-character archives", {
  expect_error(galaxias_config(archive = 100L),
               label = "`archive` should be of class `character`")
})

test_that("galaxias_config() rejects archives that do not end in `.zip`", {
  expect_error(galaxias_config(archive = "a/destination/ending/in/archive.csv"),
               label = "`archive` must specify a file name ending with `.zip`.")
})

test_that("galaxias_config() rejects archive paths that don't exist", {
  expect_error(galaxias_config(archive = "a/destination/ending/in/archive.zip"),
               label = "`archive` must specify a valid path")
})

test_that("galaxias_config() rejects non-list GBIF entries", {
  expect_error(galaxias_config(gbif = data.frame(username = "hi",
                                                 email = "there",
                                                 password = "!")))
})

test_that("galaxias_config() rejects missing GBIF entries", {
  expect_error(galaxias_config(gbif = list(username = "hi",
                                           password = "!")))
})

test_that("galaxias_config() rejects misnamed GBIF entries", {
  expect_error(galaxias_config(gbif = list(username = "hi",
                                           emil = "there",
                                           password = "!")))
})

test_that("galaxias_config() rejects GBIF entries with length > 1", {
  expect_error(galaxias_config(gbif = list(username = "hi",
                                           email = c("there", "there"),
                                           password = "!")))
})

test_that("galaxias_config() rejects non-character GBIF entries", {
  expect_error(galaxias_config(gbif = list(username = "hi",
                                           email = 22L,
                                           password = "!")))
})

test_that("galaxias_config object is correctly stored", {
  obj <- potions::pour(.pkg = "galaxias")
  expect_true(inherits(obj, "galaxias_config"))
  expect_equal(length(obj), 3)
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
  
  # reset so that later runs of `test()` don't break
  galaxias_config(directory = "data-publish",
                  gbif = list(
                    username = "",
                    email = "",
                    password = ""
                  ))
})