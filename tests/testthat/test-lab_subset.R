context("test-lab_subset")

epi_raw <- system.file("testdata", "lab_classr_epi.rds", package = "tbcleanr") %>% 
  readRDS() %>% 
  lab_classr()

epi <- lab_subset(epi_raw)


test_that("epiinfo subset correct", {
  expect_equal(class(epi_raw), class(epi))
  expect_equal(ncol(epi), 41)
  expect_equal(lab_subset(epi_raw, add = "HAINR") %>% 
               ncol(), 42)
})


k6_raw <- system.file("testdata", "lab_classr_k6.rds", package = "tbcleanr") %>% 
  readRDS() %>% 
  lab_classr()

k6 <- lab_subset(k6_raw)

test_that("koch6 subset correct", {
  expect_equal(class(k6_raw), class(k6))
})



groz_raw <- system.file("testdata", "lab_classr_groz.rds", package = "tbcleanr") %>% 
  readRDS() %>% 
  lab_classr()

groz <- lab_subset(groz_raw)


test_that("grozny subset correct", {
  expect_equal(class(groz_raw), class(groz))
})
