context("test-lab_import_checkr")
library(tbcleanr)

epi_raw <- system.file("testdata", "lab_import_epiinfo.rds", package = "tbcleanr") %>% 
  readRDS()

test_that("epiinfo works", {
  expect_error(lab_import_checkr(epi_raw), "Lab variables incorrectly parsed")
})

groz_raw <- system.file("testdata", "lab_import_grozny.rds", package = "tbcleanr") %>% 
  readRDS()

test_that("grozny works", {
  expect_error(lab_import_checkr(groz_raw), "Lab variables incorrectly parsed")
})
