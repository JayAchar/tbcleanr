context("test-lab_date_fixer")

# Epiinfo
epi_raw <- system.file("testdata", "lab_date_fixer_epi.rds", package = "tbcleanr") %>% 
  readRDS()

epi_data <- lab_date_fixer(epi_raw)

test_that("EpiInfo works fine", {
  expect_equal(ncol(epi_data), 1L)
  expect_equal(names(epi_data), "samp_date")
  expect_equal(class(epi_data), class(epi_raw))
  expect_equal(class(epi_data$samp_date), "Date")
  expect_equal(epi_data$samp_date[1], lubridate::ymd("2014-01-12"))
})



# Grozny
groz_raw <- system.file("testdata", "lab_date_fixer_groz.rds", package = "tbcleanr") %>% 
  readRDS()

groz_data <- lab_date_fixer(groz_raw)

test_that("Grozny works fine", {
  expect_equal(ncol(groz_data), 1L)
  expect_equal(names(groz_data), "samp_date")
  expect_equal(class(groz_data), class(groz_raw))
  expect_equal(class(groz_data$samp_date), "Date")
  expect_equal(groz_data$samp_date[1], lubridate::ymd("2014-01-12"))
})


test_that("Errors, warnings and messages", {
  expect_message(data.frame(x = 2) %>% lab_date_fixer(), "No lab object class")
})


## Koch 6
k6_raw <- system.file("testdata", "lab_date_fixer_k6.rds", package = "tbcleanr") %>% 
  readRDS()

k6_data <- lab_date_fixer(k6_raw)

test_that("Koch6 works fine", {
  expect_equal(ncol(k6_data), 1L)
  expect_equal(names(k6_data), "samp_date")
  expect_equal(class(k6_data), class(k6_raw))
  expect_equal(class(k6_data$samp_date), "Date")
  expect_equal(k6_data$samp_date[1], lubridate::ymd("2012-01-01"))
})
