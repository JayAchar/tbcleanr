context("test-adm_subset")
library(tbcleanr)

epi_raw <- system.file("testdata", "adm_subset_epi.rds", package = "tbcleanr") %>% 
        readRDS()

epi <- adm_subset(epi_raw)

test_that("EpiInfo output ok", {
  expect_true(! "epiinfo" %in% class(epi_raw))
  expect_true("epiinfo" %in% class(epi))
  expect_equal(dim(epi), c(nrow(epi_raw), 75))
})

# test "add" arg functionality
epi_add <- adm_subset(epi_raw, add = "SPE1")

test_that("adm_subset add functional", {
  expect_equal(ncol(epi_add), ncol(epi) + 1)
  
})

# ========= #
## Koch 6

k6_raw <- system.file("testdata", "adm_subset_koch6.rds", package = "tbcleanr") %>% 
        readRDS()

k6 <- adm_subset(k6_raw)

test_that("Koch 6 output ok", {
  expect_true(! "koch6" %in% class(k6_raw))
  expect_true("koch6" %in% class(k6))
  expect_equal(nrow(k6_raw), nrow(k6))
  expect_equal(ncol(k6), 57)
})


# test "add" arg functionality
k6_add <- adm_subset(k6_raw, add = "cdsite") 

test_that("adm_subset add functional", {
  expect_equal(ncol(k6_add), ncol(k6) + 1)
  
})
