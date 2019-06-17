context("test-adhere_classr")

# load epiinfo test data
epi <- system.file("testdata", "adhere_classr_epiinfo.rds", package = "tbcleanr") %>% 
  readRDS()

correct <- adhere_classr(epi)

test_that("epiinfo adhere_classr works", {
  # only "epiinfo" added to class
  expect_equal(dplyr::setdiff(class(correct), 
                              class(epi)), 
               "epiinfo")
  # when "epiinfo" already in class, return original data frame
  expect_equivalent(adhere_classr(correct), epi)
})


test_that("expect errors", {
  expect_warning(adhere_classr(mtcars),
                 "Data set not recognised")
  expect_error(adhere_classr(10),
               "x is not a data frame")
})