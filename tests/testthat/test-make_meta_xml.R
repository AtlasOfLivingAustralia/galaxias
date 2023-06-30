test_that("Successfully create core field in eml list template", {
  DwC_occurrence_data <- tibble(basisOfRecord =  "HUMAN_OBSERVATION",
                                scientificName = "Galaxias maculatus",
                                eventDate = "2002-12-07",
                                decimalLatitude = -33.366551,
                                decimalLongtitude = 151.47635)
  
  core <- make_core_xml(DwC_occurrence_data)
  expect_visible(core)
  expect_named(core)
  expect_equal(sum(names(core) == "field"), ncol(DwC_occurrence_data))
})
