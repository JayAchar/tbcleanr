context("test-hiv_fixer")

# Koch 6
# Load data
k6_raw <- system.file("testdata", "hiv_k6.rds", package = "tbcleanr") %>% 
        readRDS()

# generate correct output        
k6_output <- hiv_fixer(k6_raw, "koch_6")

test_that("k6 correct", {
  expect_true(all(names(k6_output) %in% c("idnum", "hiv")))
  expect_equal(nrow(k6_raw), nrow(k6_output))
  expect_true(length(unique(k6_raw$idnum)) == length(unique(k6_output$idnum)))
  expect_true(is.factor(k6_output$hiv))
  expect_true(is.character(k6_output$idnum))
  expect_equal(levels(k6_output$hiv), c("Negative", "Positive"))
})




## Epiinfo
# Load data
epi_raw <- system.file("testdata", "hiv_epi.rds", package = "tbcleanr") %>% 
        readRDS()

# generate correct output        
epi_output <- hiv_fixer(epi_raw, "epiinfo")

test_that("epiinfo correct", {
        expect_true(all(names(epi_output) %in% c("idnum", "hiv")))
        expect_equal(nrow(epi_raw), nrow(epi_output))
        expect_true(length(unique(epi_raw$idnum)) == length(unique(epi_output$idnum)))
        expect_true(is.factor(epi_output$hiv))
        expect_true(is.character(epi_output$idnum))
        expect_equal(levels(epi_output$hiv), c("Negative", "Positive"))
})

