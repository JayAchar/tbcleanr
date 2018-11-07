context("adm_subset")
library(tbcleanr)

epi_raw <- system.file("testdata", "adm_subset_epi.rds", package = "tbcleanr") %>% 
        readRDS()

epi <- adm_subset(epi_raw)

test_that("EpiInfo output ok", {
  expect_true("epiinfo" %in% class(epi))
  expect_equal(dim(epi), c(20, 74))
})


k6_raw <- system.file("testdata", "adm_subset_koch6.rds", package = "tbcleanr") %>% 
        readRDS()

koch6 <- adm_subset(k6_raw)

test_that("Koch 6 output ok", {
  expect_true("koch6" %in% class(koch6))
  expect_equal(dim(koch6), c(10, 52))
})


# remove "koch6" class
class(k6_raw) <- class(k6_raw)[1:3]

test_that("Messages, warnings and errors", {
  expect_message(adm_subset(k6_raw), "No adm object class detected")
  expect_error(adm_subset(c(1:10)), "x is not a data frame")
  expect_error(adm_subset(epi_raw, add = 1))
  expect_error(adm_subset(epi_raw, add = "random_text"), "not found")
  
})