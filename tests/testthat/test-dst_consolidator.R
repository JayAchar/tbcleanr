context("test-dst_consolidator")
library(tbcleanr)

groz_raw <- system.file("testdata", "dst_consolidator_grozny.rds", package = "tbcleanr") %>% 
  readRDS()

groz <- dst_consolidator(groz_raw, rm_orig = TRUE)

test_that("grozny works", {
  expect_equal(class(groz_raw), class(groz))
  expect_equal(nrow(groz_raw), nrow(groz))
  expect_equal(ncol(groz), 10)
  expect_equal(class(groz$dst_p_rif), "factor")
  expect_true(all(is.na(groz$dst_p_pza)))  
  expect_equal(levels(groz$dst_p_lfx), c("Sensitive", "Resistant"))
})


epi_raw <- system.file("testdata", "dst_consolidator_epiinfo.rds", package = "tbcleanr") %>% 
  readRDS()

epi <- dst_consolidator(epi_raw, rm_orig = TRUE)

test_that("epiinfo works", {
  expect_equal(class(epi_raw), class(epi))
  expect_equal(nrow(epi_raw), nrow(epi))
  expect_equal(ncol(epi), 8)
  expect_equal(class(epi$dst_p_rif), "factor")
  expect_true(all(is.na(epi$dst_p_pza)))  
  expect_equal(levels(epi$dst_p_ofx), c("Sensitive", "Resistant"))
  expect_equal(as.character(epi$dst_p_ofx), c(rep(c(rep("Resistant", 4),
                                            rep("Sensitive", 2),
                                            "Resistant",
                                            "Sensitive", NA_character_), 3)))
  expect_equal(as.character(epi$dst_p_rif),
               c(rep("Resistant", 13),
                 rep("Sensitive", 2), 
                 "Resistant",
                 rep("Sensitive", 2), 
                 rep("Resistant", 4),
                 rep("Sensitive", 2),
                 "Resistant", "Sensitive", NA_character_))
})


