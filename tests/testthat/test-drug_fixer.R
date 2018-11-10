context("test-drug_fixer")
library(tbcleanr)

# Load data
k6_raw <- system.file("testdata", "drug_k6.rds", package = "tbcleanr") %>% 
    readRDS()

k6_out <- drug_fixer(k6_raw)

test_that("koch6 correct", {
    expect_equal(class(k6_raw), class(k6_out))
    expect_equal(dim(k6_raw), dim(k6_out))
    expect_equal(k6_out$E %>% levels() %>% length(), 2)
})


# load epiinfo data
epi_raw <- system.file("testdata", "drug_epi.rds", package = "tbcleanr") %>% 
    readRDS()

epi_out <- drug_fixer(epi_raw)

test_that("epiinfo correct", {
    expect_equal(class(epi_raw), class(epi_out))
    expect_equal(dim(epi_raw), dim(epi_out))
    expect_equal(epi_out$ZDZ %>% levels() %>% length(), 2)
})
