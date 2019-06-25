context("test-outcome_fixer")
library(tbcleanr)


# load k6 file
k6 <- system.file("testdata", "outcome_k6.rds", package = "tbcleanr") %>% 
    readRDS()

k6_out <- outcome_fixer(k6, 
                        who_defined = TRUE,
                        bin_outcome = TRUE,
                        rm_orig = TRUE)

test_that("koch6 correct", {
    expect_true(class(k6) == "data.frame")
    expect_true("koch6" %in% class(k6_out))
    expect_equal(nrow(k6), nrow(k6_out))
    expect_equal(outcome_fixer(k6, rm_orig = F) %>% ncol(),
                 ncol(k6_out) + 3)
    expect_equal(class(k6_out$outcome), "factor")
    expect_equal(k6_out$outcome %>% levels() %>% length(),
                 9)
    expect_equal(k6_out %>% 
                     dplyr::count(outcome) %>% 
                     dplyr::filter(outcome == "Death") %>% 
                     dplyr::pull(), 18)
    expect_true(k6_out$outcome[146] == "Cured")
    expect_true(class(k6_out$outcome_who) == "factor")
    expect_true("outcome_who" %in% names(k6_out))
    expect_true(ncol(outcome_fixer(k6, who_defined = FALSE)) == ncol(k6_out) -1)
    expect_true("outcome_bin" %in% names(k6_out))
    expect_true(class(k6_out$outcome_bin) == "factor")
})


# load epiinfo file
epi <- system.file("testdata", "outcome_epi.rds", package = "tbcleanr") %>% 
    readRDS()

epi_out <- outcome_fixer(epi,
                         who_defined = TRUE,
                         bin_outcome = TRUE, 
                         rm_orig = TRUE)

test_that("epiinfo correct", {
    expect_true(class(epi) == "data.frame")
    expect_true("epiinfo" %in% class(epi_out))
    expect_equal(nrow(epi), nrow(epi_out))
    expect_equal(outcome_fixer(epi, rm_orig = FALSE) %>% ncol(),
                 ncol(epi_out) + 1)
    expect_equal(class(epi_out$outcome), "factor")
    expect_equal(epi_out$outcome %>% levels() %>% length(),
                 9)
    expect_true(epi_out$outcome[2] == "Cured") 
    expect_true("outcome_who" %in% names(epi_out))
    expect_true(class(epi_out$outcome_who) == "factor")
    expect_true("outcome_bin" %in% names(epi_out))
    expect_true(class(epi_out$outcome_bin) == "factor")
    expect_equal(sum(k6_out$outcome_bin == "Treatment successful", na.rm = TRUE),
                 sum(k6_out$outcome %in% c("Cured", "Completed"), na.rm = TRUE))
    
})



test_that("errors & messages", {
    expect_error(epi %>% 
                     dplyr::rename("outcome" = RES) %>% 
                     outcome_fixer(), "Check all outcome variables are included in data frame")
    expect_error(epi %>%
                     list() %>% 
                     outcome_fixer(), "x is not a data frame")
    expect_error(outcome_fixer(epi, who_defined = 1))
})



