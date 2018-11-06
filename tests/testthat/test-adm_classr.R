context("adm_classr")
library(tbcleanr)

epi_raw <- system.file("testdata", "epi_adm_classr.rds", package = "tbcleanr") %>% 
        readRDS()

epi_data <- adm_classr(epi_raw)

test_that("epiinfo database", {
  expect_equal(dim(epi_raw), dim(epi_data))
  expect_equal(names(epi_raw), names(epi_data))
  expect_true("epiinfo" %in% class(epi_data))
})


k6_raw <- system.file("testdata", "k6_adm_classr.rds", package = "tbcleanr") %>% 
        readRDS()

k6_data <- adm_classr(k6_raw)

test_that("koch6 database", {
        expect_equal(dim(k6_raw), dim(k6_data))
        expect_equal(names(k6_raw), names(k6_data))
        expect_true("koch6" %in% class(k6_data))
})


test_that("errors and messages", {
  expect_error(adm_classr(c("a", "b")), "x is not a data frame")
  expect_warning(adm_classr(data.frame(a = 1:4, b = c("a", "b", "c", "d"))))        

})