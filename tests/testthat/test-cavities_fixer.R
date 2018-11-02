context("cavities_fixer")
library(tbcleanr)

# load data
  data_k6 <- system.file("testdata", "cav_k6.rds", package = "tbcleanr") %>% 
                readRDS()

# k6 output
  k6 <- cavities_fixer(data_k6, "koch_6", "chechnya", rm_orig = FALSE)
  
test_that("k6 output structure", {
  expect_equal(class(k6), "data.frame")
  expect_equal(nrow(data_k6), nrow(k6))
  expect_equal(ncol(data_k6), ncol(k6) - 1)
  expect_equal(length(unique(data_k6$id)), length(unique(k6$id)))
})
