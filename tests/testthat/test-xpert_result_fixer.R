context("test-xpert_result_fixer")

groz_raw <- system.file("testdata", "xpert_grozny.rds", package = "tbcleanr") %>% 
  readRDS() 

groz <- xpert_result_fixer(groz_raw, rm_orig = FALSE)

test_that("grozny correct", {
  expect_equal(ncol(groz_raw) + 2, ncol(groz))
  expect_equal(nrow(groz_raw), nrow(groz))
  expect_true(groz$xpert1err %>% unique() %in% c(0, 1) %>% all())
  expect_true(groz$xpert2err %>% unique() %in% c(0, 1) %>% all())
  expect_true(groz$xpert1res %>% unique() %in% c(0, 1) %>% all())
  expect_true(groz$xpert2res %>% unique() %in% c(0, 1) %>% all())
  expect_true(groz$xpert1rif %>% unique() %in% c(0, 1, NA_integer_) %>% all())
  expect_true(groz$xpert2rif %>% unique() %in% c(0, 1, NA_integer_) %>% all())
  # no rif results when Xpert negative
  expect_equal(sum(groz$xpert_res == "Negative" & 
                     groz$xpert_rif %in% c("Not detected", "Detected"), na.rm = T), 0)
})
