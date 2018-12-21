context("test-lab_date_fixer")


test_that("Errors, warnings and messages", {
  expect_message(data.frame(x = 2) %>% lab_date_fixer(), "No lab object class")
})
