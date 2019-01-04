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

# Newer lab data set with new Xpert variable names
epi_new <- epi_raw %>% 
  dplyr::select(-GX_res4, -GX_date4) %>% 
  dplyr::rename(GeneX_RES1 = GX_res1,
         GeneX_RES2 = GX_res2,
         GeneX_RES3 = GX_res3)

epi_new_subset <- lab_subset(epi_new)

test_that("new epiinfo xpert subset correct", {
  expect_equal(class(epi_new_subset), class(epi_new))
  expect_equal(ncol(epi_new_subset), 40)
  expect_equal(lab_subset(epi_new, add = "HAINR") %>% 
                 ncol(), 41)
})

# ================================

k6_raw <- system.file("testdata", "lab_classr_k6.rds", package = "tbcleanr") %>% 
  readRDS() %>% 
  lab_classr()

k6 <- lab_subset(k6_raw)

test_that("koch6 subset correct", {
  expect_equal(class(k6_raw), class(k6))
})



groz_raw <- system.file("testdata", "lab_subset_groz.rds", package = "tbcleanr") %>% 
  readRDS()

groz <- lab_subset(groz_raw)


test_that("grozny subset correct", {
  expect_equal(class(groz_raw), class(groz))
  expect_true("dstnumber" %in% names(groz))
  expect_equal(nrow(groz_raw), nrow(groz))
  expect_false(stringr::str_detect(groz$dstnumber, pattern = "-"))
})
