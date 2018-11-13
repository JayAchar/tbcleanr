context("test-lab_classr")
library(tbcleanr)

epi_raw <- system.file("testdata", "lab_classr_epi.rds", package = "tbcleanr") %>% 
  readRDS()

epi <- lab_classr(epi_raw)

test_that("lab epiinfo works correctly", {
  expect_equal(dim(epi_raw), dim(epi))
  expect_equal(names(epi_raw), names(epi))
  expect_true("epiinfo" %in% class(epi))
  })



k6_raw <- system.file("testdata", "lab_classr_k6.rds", package = "tbcleanr") %>% 
  readRDS()

k6 <- lab_classr(k6_raw)

test_that("lab k6 works correctly", {
  expect_equal(dim(k6_raw), dim(k6))
  expect_equal(names(k6_raw), names(k6))
  expect_true("koch6" %in% class(k6))
})



groz_raw <- system.file("testdata", "lab_classr_groz.rds", package = "tbcleanr") %>% 
  readRDS()

groz <- lab_classr(groz_raw)

test_that("lab grozny works correctly", {
  expect_equal(dim(groz_raw), dim(groz))
  expect_equal(names(groz_raw), names(groz))
  expect_true("grozny" %in% class(groz))
})


error_df <- data.frame(x = "test", y = "foo", stringsAsFactors = F)

test_that("error correctly", {
  expect_warning(lab_classr(error_df), 
                 "Data set not recognised - no object class assigned")
})

