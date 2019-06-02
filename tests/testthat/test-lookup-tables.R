context("test-lookup-tables")
library(tbcleanr)

test_that("oca-missions", {
  expect_true(is.character(oca_missions))
  expect_true(length(unique(oca_missions)) < length(names(oca_missions)))
})

test_that("oca-projects", {
  expect_true(is.character(oca_projects))
  expect_equal(oca_projects[["0"]], "Monrovia")
  expect_true(length(unique(oca_projects)) < length(names(oca_projects)))
})
