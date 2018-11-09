context("test-hiv_fixer")

# Koch 6
# Load data
k6_raw <- system.file("testdata", "hiv_k6.rds", package = "tbcleanr") %>% 
        readRDS()

# generate correct output        
k6_output <- hiv_fixer(k6_raw)

test_that("k6 correct", {
  expect_true(all(names(k6_output) %in% c("registrationnb", "hiv")))
  expect_equal(nrow(k6_raw), nrow(k6_output))
  expect_true(length(unique(k6_raw$registrationnb)) == length(unique(k6_output$registrationnb)))
  expect_true(is.factor(k6_output$hiv))
  expect_true(is.character(k6_output$registrationnb))
  expect_equal(levels(k6_output$hiv), c("Negative", "Positive"))
  expect_equal(class(k6_raw), class(k6_output))
  expect_true(is.na(k6_output$hiv[1]))
  expect_true(k6_output$hiv[7] == "Positive")
  expect_true(k6_output$hiv[39] == "Negative")
})




## Epiinfo
# Load data
epi_raw <- system.file("testdata", "hiv_epi.rds", package = "tbcleanr") %>% 
        readRDS()

# generate correct output        
epi_output <- hiv_fixer(epi_raw)

test_that("epiinfo correct", {
    expect_true(all(names(epi_output) %in% c("APID", "HIV")))
    expect_equal(nrow(epi_raw), nrow(epi_output))
    expect_true(length(unique(epi_raw$APID)) == length(unique(epi_output$APID)))
    expect_true(is.factor(epi_output$HIV))
    expect_true(is.character(epi_output$APID))
    expect_equal(levels(epi_output$HIV), c("Negative", "Positive"))
    expect_equal(class(epi_raw), class(epi_output))
    
})

