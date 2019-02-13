context("test-adm_subset")
library(tbcleanr)

epi_raw <- system.file("testdata", "adm_subset_epi.rds", package = "tbcleanr") %>% 
        readRDS()

epi <- adm_subset(epi_raw)

test_that("EpiInfo output ok", {
  expect_true("epiinfo" %in% class(epi))
  expect_equal(dim(epi), c(20, 75))
})


k6_raw <- system.file("testdata", "adm_subset_koch6.rds", package = "tbcleanr") %>% 
        readRDS()

koch6 <- adm_subset(k6_raw)

test_that("Koch 6 output ok", {
  expect_true(! "koch6" %in% class(k6_raw))
  expect_true("koch6" %in% class(koch6))
  expect_equal(nrow(k6_raw), nrow(koch6))
  expect_equal(ncol(koch6), 55)
})


# remove "koch6" class
class(k6_raw) <- class(k6_raw)[1:3]

test_that("Messages, warnings and errors", {
  expect_error(adm_subset(c(1:10)), "x is not a data frame")
  expect_error(adm_subset(epi_raw, add = 1))
  expect_error(adm_subset(epi_raw, add = "random_text"))
  
})