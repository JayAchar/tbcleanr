context("test-result_consolidator")
library(tbcleanr)

groz_raw <- system.file("testdata", "result_consolidator_groz.rds", package = "tbcleanr") %>% 
  readRDS()

groz <- result_consolidator(groz_raw, rm_orig = FALSE)

test_that("grozny works", {
  expect_equal(class(groz_raw), class(groz))
  expect_equal(nrow(groz_raw), nrow(groz))
  expect_equal(ncol(groz_raw) + 2, ncol(groz))
  expect_equal(class(groz$smear), c("ordered", "factor"))
  expect_equal(groz$smear[1] %>% as.character(), "Negative")
  expect_equal(groz$smear[5] %>% as.character(), "3+")
  expect_equal(groz$smear[6] %>% as.character(), "Scanty")
  expect_equal(class(groz$culture), c("ordered", "factor"))
  expect_equal(levels(groz$culture), c("Negative", "Positive"))
  expect_equal(groz$culture[1] %>% as.character(), "Negative")
  expect_equal(groz$culture[2] %>% as.character(), "Positive")
  expect_equal(groz$culture[13] %>% as.character(), NA_character_)
  })



epi_raw <- system.file("testdata", "result_consolidator_epiinfo.rds", package = "tbcleanr") %>% 
  readRDS()

epi <- result_consolidator(epi_raw, rm_orig = FALSE)

test_that("epiinfo works", {
  expect_equal(class(epi_raw), class(epi))
  expect_equal(nrow(epi_raw), nrow(epi))
  expect_equal(ncol(epi_raw) + 2, ncol(epi))
  expect_equal(class(epi$smear), c("ordered", "factor"))
  expect_equal(epi$smear[1] %>% as.character(), "Negative")
  expect_equal(epi$smear[4] %>% as.character(), "3+")
  expect_equal(epi$smear[5] %>% as.character(), "Scanty")
  expect_equal(class(epi$culture), c("ordered", "factor"))
  expect_equal(levels(epi$culture), c("Negative", "Positive"))
  expect_equal(epi$culture[1] %>% as.character(), "Negative")
  expect_equal(epi$culture[2] %>% as.character(), "Positive")
  expect_equal(epi$culture[27] %>% as.character(), NA_character_)
})