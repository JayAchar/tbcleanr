context("test-recorded_dst")

epi <- system.file("testdata", "recorded_dst_epiinfo.rds", package = "tbcleanr") %>% 
  readRDS()

correct <- recorded_dst(epi)

test_that("epiinfo works", {
  expect_equal(class(epi), class(correct))
  expect_equal(class(correct$DIPRO), "factor")
  expect_equal(length(levels(correct$DIPRO)), 7)
})

# =====

# koch 6

k6 <- system.file("testdata", "recorded_dst_koch6.rds", package = "tbcleanr") %>% 
  readRDS()

correct <- recorded_dst(k6)

test_that("koch6 works", {
  expect_equal(class(k6), class(correct))
  expect_equal(class(correct$recorded_dst), "factor")
  expect_equal(length(levels(correct$recorded_dst)), 7)
})

