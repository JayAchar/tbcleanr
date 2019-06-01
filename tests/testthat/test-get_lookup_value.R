context("test-get_lookup_value")

test_that("normal functioning works", {
  expect_equal(get_lookup_value("DRC", table_name = oca_missions), "DRC_Katanga")
  expect_true(is.na(get_lookup_value("Rubbish String", table_name = oca_missions)))
  expect_error(get_lookup_value("DRC", table_name = test_table))
  expect_error(get_lookup_value("DRC", table_name = c(test_table, test_table2)))
  expect_error(get_lookup_value(DRC, table_name = oca_missions))
})
