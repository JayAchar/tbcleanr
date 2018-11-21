context("test-xpert_result_fixer")

groz_raw <- system.file("testdata", "xpert_grozny.rds", package = "tbcleanr") %>% 
  readRDS() 

groz <- xpert_result_fixer(groz_raw, rm_orig = FALSE)

test_that("grozny correct", {
  expect_equal(ncol(groz_raw) + 2, ncol(groz))
  expect_equal(nrow(groz_raw), nrow(groz))
})
