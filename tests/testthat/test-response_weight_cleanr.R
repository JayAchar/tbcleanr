context("test-response_weight_cleanr")
library(tbcleanr)

input <- system.file("testdata", "response_wt_cleanr_epiinfo.rds", package = "tbcleanr") %>% 
  readRDS()

output <- response_weight_cleanr(input)

test_that("Epiinfo works", {
  expect_equal(class(input), class(output))
  expect_equal(sum(is.na(output$WEIGHT)), 0L)
  expect_equal(nrow(output), 
               length(unique(output$tx_month)))
  expect_equal(output$WEIGHT[2], 31)
  expect_equal(output$WEIGHT[7], 47)
})
