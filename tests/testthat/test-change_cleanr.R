context("test-change_cleanr")
library(tbcleanr)

### EpiInfo
# Load data
input <- system.file("testdata", "change_cleanr_epiinfo.rds", package = "tbcleanr") %>% 
  readRDS()

# generate correct output        
output <- change_cleanr(input)


test_that("epiinfo correct", {
  expect_equal(class(input), class(output))
  expect_equal(nrow(input), nrow(output) + 1)
  expect_true(class(output$change_dt) == "Date")
  expect_true(class(output$bdq_change) == "factor")
  expect_equal(levels(output$bdq_change), c("Start", "Stop"))
  expect_true(output$bdq_change[3] == "Start")
  expect_true(output$cfz_change[5] %>% is.na())
  expect_true(output$dlm_change[1] == "Stop")
  expect_true(output$lzd_change[7] == "Start")
  })

### Koch6
# Load data
input <- system.file("testdata", "change_cleanr_koch6.rds", package = "tbcleanr") %>% 
  readRDS()

# generate correct output        
output <- change_cleanr(input)


test_that("koch6 correct", {
  expect_equal(class(input), class(output))
  expect_equal(nrow(input), nrow(output) + 1)
  expect_true(class(output$change_dt) == "Date")
  expect_true(class(output$bdq_change) == "factor")
  expect_equal(levels(output$bdq_change), c("Start", "Stop"))
  expect_true(output$bdq_change[3] == "Start")
  expect_true(output$cfz_change[5] %>% is.na())
  expect_true(output$dlm_change[1] == "Stop")
  expect_true(output$lzd_change[7] == "Start")
})
