context("test-outcome_fixer")
library(tbcleanr)


# load k6 file
k6 <- system.file("testdata", "outcome_k6.rds", package = "tbcleanr") %>% 
    readRDS()

k6_out <- outcome_fixer(k6)

test_that("koch6 correct", {
    expect_equal(class(k6), class(k6_out))
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
})


# load epiinfo file
epi <- system.file("testdata", "outcome_epi.rds", package = "tbcleanr") %>% 
    readRDS()

epi_out <- outcome_fixer(epi)

test_that("epiinfo correct", {
    expect_equal(class(epi), class(epi_out))
    expect_equal(nrow(epi), nrow(epi_out))
    expect_equal(outcome_fixer(epi, rm_orig = F) %>% ncol(),
                 ncol(epi_out) + 1)
    expect_equal(class(epi_out$outcome), "factor")
    expect_equal(epi_out$outcome %>% levels() %>% length(),
                 9)
    expect_true(epi_out$outcome[2] == "Cured")    
})



test_that("errors & messages", {
    expect_error(epi %>% 
                     dplyr::rename("outcome" = RES) %>% 
                     outcome_fixer(), "Check all outcome variables are included in data frame")
    expect_error(epi %>%
                     list() %>% 
                     outcome_fixer(), "x is not a data frame")
})



