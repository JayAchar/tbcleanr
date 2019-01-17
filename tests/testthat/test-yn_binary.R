context("test-yn_binary")

data <- system.file("testdata", "yn_binary.rds", package = "tbcleanr") %>% 
  readRDS()

# test numeric variable       
int_out <- tbcleanr:::yn_binary_fixer(data$ALCO)

test_that("yn_binary int works", {
  expect_equal(length(data$ALCO), length(int_out))
  expect_equal(class(int_out), "factor")
  expect_equal(levels(int_out), c("No", "Yes"))
  expect_equal(int_out[1:4], factor(c("Yes", "No", "Yes", "No")))
})

# test character variable
chr_out <- tbcleanr:::yn_binary_fixer(data$INJECT)

test_that("yn_binary chr works", {
  expect_equal(length(data$INJECT), length(chr_out))
  expect_equal(class(chr_out), "factor")
  expect_equal(levels(chr_out), c("No", "Yes"))
  expect_equal(chr_out[1:4], factor(c("No", "Yes", "No", "Yes")))
})

# test error message with logical variable

test_that("yn_binary lgl works", {
  expect_error(tbcleanr:::yn_binary_fixer(data$TEST))
  expect_message(tbcleanr::yn_binary_fixer(data$ALL_NA))

})