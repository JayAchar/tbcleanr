context("test-date_format")
library(tbcleanr)

epi <- system.file("testdata", "adm_subset_epi.rds", package = "tbcleanr") %>% 
        readRDS() %>% 
        date_format()

test_that("correct output", {
  expect_equal(class(epi$BIRTDATE), "Date")
  expect_equal(epi$STARTTRE[1], lubridate::dmy("08/01/2004"))
})
