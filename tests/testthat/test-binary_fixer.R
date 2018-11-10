context("test-binary_fixer")
library(tbcleanr)

# load koch 6 data
k6_raw <- system.file("testdata", "binary_k6.rds", package = "tbcleanr") %>% 
    readRDS()

k6_data <- binary_fixer(k6_raw)

test_that("k6 correct", {
    expect_equal(dim(k6_raw), dim(k6_data))
    expect_equal(class(k6_raw), class(k6_data))
    expect_true(is.numeric(k6_data$test_var))
    expect_true(is.factor(k6_data$diabetes))
    expect_true(k6_data$diabetes[3] %>% is.na())
    })

# load EpiInfo data
epi_raw <- system.file("testdata", "binary_epi.rds", package = "tbcleanr") %>% 
    readRDS()

epi_data <- binary_fixer(epi_raw)

test_that("epiinfo correct", {
    expect_equal(dim(epi_raw), dim(epi_data))
    expect_equal(class(epi_raw), class(epi_data))
    expect_true(is.numeric(epi_data$test_var))
    expect_true(is.factor(epi_data$DIABETES))
    expect_true(epi_data$DIABETES[3] %>% is.na())
})               