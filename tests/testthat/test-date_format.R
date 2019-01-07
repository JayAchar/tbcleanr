context("test-date_format")
library(tbcleanr)

chr_str <- system.file("testdata", "date_format_chr_str.rds", package = "tbcleanr") %>% 
        readRDS()

chr_str_out <- date_format(chr_str)

test_that("dates all character strings output", {
  expect_equal(dim(chr_str), dim(chr_str_out))
  expect_equal(names(chr_str), names(chr_str_out))
  expect_equal(class(chr_str_out$FIRST), "Date")
  expect_equal(class(chr_str_out$CULRES), "Date")
  
})

posix <- system.file("testdata", "date_format_posix.rds", package = "tbcleanr") %>% 
  readRDS()

posix_out <- date_format(posix)

test_that("dates all posix class", {
  expect_equal(dim(posix), dim(posix_out))
  expect_equal(names(posix), names(posix_out))
  expect_equal(class(posix_out$FIRST), "Date")
  expect_equal(class(posix_out$CULRES), "Date")
})


chr_posix <- system.file("testdata", "date_format_chr_posix.rds", package = "tbcleanr") %>% 
  readRDS()

chr_posix_out <- date_format(chr_posix)

test_that("dates mixed posix and characters", {
  expect_equal(dim(chr_posix), dim(chr_posix_out))
  expect_equal(names(chr_posix), names(chr_posix_out))
  expect_equal(class(chr_posix_out$FIRST), "Date")
  expect_equal(class(chr_posix_out$CULRES), "Date")
})
