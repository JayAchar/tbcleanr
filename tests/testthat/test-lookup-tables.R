context("test-lookup-tables")
library(tbcleanr)

test_that("oca-missions", {
  expect_true(is.character(oca_missions))
})
