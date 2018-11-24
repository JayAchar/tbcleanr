context("test-lab_sample_fixer")

groz_raw <- system.file("testdata", "lab_sample_fixer_grozny.rds", 
                        package = "tbcleanr") %>% 
  readRDS() 

groz <- lab_sample_fixer(groz_raw, rm_orig = FALSE)

test_that("grozny works", {
  expect_equal(nrow(groz_raw), nrow(groz))
  expect_equal(ncol(groz_raw) + 1, ncol(groz))
  expect_identical(class(groz$sample), "factor")
  expect_equal(levels(groz$sample), c("Sputum", "EP sample"))
  expect_identical(class(groz_raw), class(groz))
})
