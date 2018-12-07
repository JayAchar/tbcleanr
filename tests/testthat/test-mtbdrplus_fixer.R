context("test-mtbdrplus_fixer")
library(tbcleanr)

epi_raw <- system.file("testdata", "mtbdr_epiinfo.rds", package = "tbcleanr") %>% 
  readRDS() 

epi <- mtbdrplus_fixer(epi_raw)

test_that("epiinfo works", {
  expect_equal(dim(epi_raw), dim(epi))
  expect_equal(class(epi_raw), class(epi))
  expect_equal(names(epi), c("hain_inh", "hain_rif"))
  expect_equal(class(epi$hain_inh), "factor")
  expect_equal(epi$hain_rif[1] %>% as.character(), "Resistant")
  expect_equal(epi$hain_rif[2] %>% as.character(), "Sensitive")
  expect_equal(epi$hain_rif[3] %>% as.character(), NA_character_)
})


groz_raw <- system.file("testdata", "mtbdr_grozny.rds", package = "tbcleanr") %>% 
  readRDS() 

groz <- mtbdrplus_fixer(groz_raw)

test_that("grozny works", {
  expect_equal(dim(groz_raw), dim(groz))
  expect_equal(class(groz_raw), class(groz))
  expect_true(all(c("hain_inh", "hain_rif", "hain_res") %in% names(groz)))
  expect_equal(class(groz$hain_inh), "factor")
  expect_equal(groz$hain_rif[1] %>% as.character(), "Sensitive")
  expect_equal(groz$hain_rif[2] %>% as.character(), "Resistant")
  expect_equal(groz$hain_rif[3] %>% as.character(), NA_character_)
  expect_equal(groz$hain_res[1] %>% as.character(), "Positive")
  expect_equal(groz$hain_res[2] %>% as.character(), "Negative")
  })
