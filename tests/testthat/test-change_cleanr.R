context("test-change_cleanr")
library(tbcleanr)

# define unit tests code
testing_code <- quote({
  expect_true("data.frame" %in% class(output))
  expect_true("bdq_change" %in% names(output))
  
  expect_true(output$ofx_change[output[[1]] == "XYZ1"] == "Stop")
  expect_true(output$ofx_change[output[[1]] == "XYZ2"] == "Start")
  expect_false(all("XYZ3" %in% output[[1]]))
  expect_false(all("XYZ4" %in% output[[1]]))
  
  if (all(c("STARTTRE", "DACHAN", "APID") %in% names(input))) {
    expect_true("STARTTRE" %in% names(change_cleanr(input, add = "STARTTRE")))
  }
  
  if (all(c("reason", "SEdescrip", "RegistrationNb") %in% names(input))) {
    expect_true("reason" %in% names(change_cleanr(input, add = "reason")))
  }
  
})

error_testing <- quote({
  # add arg not present in data frame
  expect_warning(change_cleanr(input, add = "REGRP"))
  # add arg must be string
  expect_error(change_cleanr(input, add = 12))
})

## Epiinfo
# load test data
input <- system.file("testdata", "change_cleanr_epi.rds", package="tbcleanr") %>% 
  readRDS()

output <- tbcleanr:::change_cleanr(x = input)

# test code
test_that("EpiInfo testing", eval(testing_code))
test_that("EpiInfo errors", eval(error_testing))

## Koch6
input <- system.file("testdata", "change_cleanr_koch6.rds", package = "tbcleanr") %>%
  readRDS()

# generate correct output
output <- change_cleanr(input)

# test code
test_that("Koch6 testing", eval(testing_code))
test_that("Koch6 errors", eval(error_testing))
