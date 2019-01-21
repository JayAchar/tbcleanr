context("test-txhistory")

epi <- system.file("testdata", "txhistory_epiinfo.rds", package = "tbcleanr") %>% 
  readRDS()

correct <- txhistory(epi)

test_that("epiinfo works", {
  expect_equal(class(epi), class(correct))
  expect_equal(class(correct$REGRP), "factor")
  expect_equal(length(levels(correct$REGRP)), 7)
})

# =====

# koch 6

k6 <- system.file("testdata", "txhistory_koch6.rds", package = "tbcleanr") %>% 
  readRDS()

correct <- txhistory(k6)

test_that("koch6 works", {
  expect_equal(class(k6), class(correct))
  expect_equal(class(correct$cdhistory), "factor")
  expect_equal(length(levels(correct$cdhistory)), 5)
})

