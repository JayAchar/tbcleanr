context("test-drug_adhere")
library(tbcleanr)

# define unit tests code
testing_code <- quote({
  expect_true("data.frame" %in% class(output))
  expect_true("adhere_pct_H" %in% names(output))
  expect_true(is.numeric(output$adhere_pct_H))
  # unique combination of ID number and month
  expect_equal(nrow(output), nrow(unique(output[ , c(1, 2)])))
  
  
  expect_true(all(! is.na(output$tx_month)))
  expect_true(all(! is.na(output$APID)))
  
  expect_true(output$adhere_pct_H[output[[1]] == "XYZ1" &
                                    output[[2]] == 1] == (8/12 * 100))
  expect_true(is.na(output$adhere_pct_H[output[[1]] == "XYZ2" &
                                    output[[2]] == 1]))
  expect_true(output$adhere_pct_H[output[[1]] == "XYZ3" &
                                    output[[2]] == 1] == 0L)
  expect_true(output$adhere_pct_H[output[[1]] == "XYZ4" &
                                    output[[2]] == 1] == (8/12 * 100))
  expect_true(output$adhere_pct_H[output[[1]] == "XYZ4" &
                                    output[[2]] == 2] == (10/12 * 100))
  expect_true(is.na(output$adhere_pct_H[output[[1]] == "XYZ5" &
                                          output[[2]] == 1]))
    
})

error_testing <- quote({

})

## Epiinfo
# load test data
input <- system.file("testdata", "drug_adhere_epi.rds", package="tbcleanr") %>% 
  readRDS() 

output <- tbcleanr:::drug_adhere(df = input,
                                 drug = "H")

# test code
test_that("EpiInfo testing", eval(testing_code))
# test_that("EpiInfo errors", eval(error_testing))
