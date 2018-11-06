context("adm_subset")
library(tbcleanr)

epi_raw <- system.file("testdata", "adm_subset_epi.rds", package = "tbcleanr") %>% 
        readRDS()

epi <- adm_subset(epi_raw)

test_that("EpiInfo output ok", {
  expect_true("epiinfo" %in% class(epi))
  expect_equal(dim(epi), c(20, 74))
})


k6_raw <- system.file("testdata", "adm_subset_koch6.rds", package = "tbcleanr") %>% 
        readRDS()

koch6 <- adm_subset(k6_raw)

test_that("Koch 6 output ok", {
        expect_true("koch6" %in% class(koch6))
        expect_equal(dim(koch6), c(10, 52))
})