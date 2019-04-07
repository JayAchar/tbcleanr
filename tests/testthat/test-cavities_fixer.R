context("test-cavities_fixer")


# Load data
epi_raw <- system.file("testdata", "cavities_fixer_epiinfo.rds", package = "tbcleanr") %>% 
  readRDS()

k6_raw <- system.file("testdata", "cavities_fixer_koch6.rds", package = "tbcleanr") %>% 
  readRDS()

# Function calls

epi <- cavities_fixer(epi_raw)

k6 <- cavities_fixer(k6_raw)


test_that("EpiInfo", {
  expect_equal(nrow(epi_raw), nrow(epi))
  expect_equal(ncol(epi_raw), ncol(epi) - 2)
  expect_true(is.factor(epi$xray_result))
  expect_true(is.factor(epi$cavity))
  expect_true(all(epi$xray_result[epi_raw$XRAYRES == 1L] == "Normal"))
  expect_true(all(epi$xray_result[epi_raw$XRAYRES == 2L] == "Abnormal"))
  expect_true(all(is.na(epi$xray_result[epi_raw$XRAYRES %in% c(3, 4)])))
  
  expect_true(all(epi$cavity[epi_raw$ABNORM == 1L] == "Cavitary"))
  expect_true(all(epi$cavity[epi_raw$ABNORM == 2L] == "Non-cavitary"))
})


test_that("Koch6", {
  expect_equal(nrow(k6_raw), nrow(k6))
  expect_equal(ncol(k6_raw), ncol(k6) - 2)
  
  expect_true(all(k6$xray_result[k6_raw$XRay_Finding == 1L] == "Normal"))
  expect_true(all(k6$xray_result[k6_raw$XRay_Finding == 2L] == "Abnormal"))
  
  expect_true(all(k6$cavity[k6_raw$cavsimple == 1L] == "Cavitary"))
  expect_true(all(k6$cavity[k6_raw$cav %in% c(1L, 2L)] == "Cavitary"))
  expect_true(all(k6$cavity[k6_raw$cavD %in% c(1L, 2L)] == "Cavitary"))
  
  expect_true(all(k6$cavity[k6_raw$cav == 0L &
                              k6_raw$cavD == 0L &
                              (is.na(k6_raw$cavsimple) |
                                 k6_raw$cavsimple == 2L)] == "Non-cavitary"))
})