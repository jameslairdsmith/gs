context("test_month_functions")

test_that("in_month function work", {

  in_july <- in_month("Jul")

  expect_true(test_date(dmy("12/07/1990"), in_july))
  expect_true(test_date(dmy("13/07/1990"), in_july))
  expect_false(test_date(dmy("12/06/1990"), in_july))
  expect_false(test_date(dmy("13/06/1990"), in_july))
})
