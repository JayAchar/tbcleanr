context("test-r-gender_fixer")
library(tbcleanr)

# Load data
        k6_raw <- system.file("testdata", "gender_k6.rds", package = "tbcleanr") %>% 
                        readRDS()

# generate correct output        
        k6_output <- gender_fixer(k6_raw, "koch_6")

test_that("k6 correct", {
  expect_true(all(names(k6_output) %in% c("idnum", "gender")))
  expect_equal(nrow(k6_raw), nrow(k6_output))
  expect_true(length(unique(k6_raw$idnum)) == length(unique(k6_output$idnum)))
  expect_true(is.factor(k6_output$gender))
  expect_true(is.character(k6_output$idnum))
  expect_equal(levels(k6_output$gender), c("Male", "Female"))
  expect_true(all(k6_output$gender[k6_raw$gender == 1] == "Male"))
  expect_true(all(k6_output$gender[k6_raw$gender == 2] == "Female"))
})

# generate raw data with NA
        k6_NA <- rbind(k6_raw, 
                       data.frame(idnum = as.character(11), 
                                  gender = NA_integer_, 
                                  stringsAsFactors = F))
# generate NA output
        k6_na_output <- gender_fixer(k6_NA, "koch_6")

test_that("k6 NA correct", {
        expect_equal(nrow(k6_NA), nrow(k6_na_output))
        expect_equal(levels(k6_output$gender), c("Male", "Female"))
        expect_true(is.na(k6_na_output$gender[which(is.na(k6_NA$gender))]))
})

# generate raw data with additional gender level
        k6_abn_level <- rbind(k6_raw, 
                       data.frame(idnum = as.character(11), 
                                  gender = as.numeric(3), 
                                  stringsAsFactors = F))
# test abnormal gender levels
test_that("k6 abnormal gender", {
        expect_error(gender_fixer(k6_abn_level, "koch_6"), 
                     "Gender variable does not have 2 levels")
})
