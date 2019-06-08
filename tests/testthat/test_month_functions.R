context("test_month_functions")

test_that("in_month function work", {

  in_july <- in_month("Jul")

  expect_true(test_date(dmy("12/07/1990"), in_july))
  expect_true(test_date(dmy("13/07/1990"), in_july))
  expect_false(test_date(dmy("12/06/1990"), in_july))
  expect_false(test_date(dmy("13/06/1990"), in_july))
})

test_that("in_month function works with numbers", {

  in_july <- in_month(7)

  expect_true(test_date(dmy("12/07/1990"), in_july))
  expect_true(test_date(dmy("13/07/1990"), in_july))
  expect_false(test_date(dmy("12/06/1990"), in_july))
  expect_false(test_date(dmy("13/06/1990"), in_july))
})

test_that("in_month function works with string numbers", {

  in_july <- in_month("7")

  expect_true(test_date(dmy("12/07/1990"), in_july))
  expect_true(test_date(dmy("13/07/1990"), in_july))
  expect_false(test_date(dmy("12/06/1990"), in_july))
  expect_false(test_date(dmy("13/06/1990"), in_july))
})

test_that("in_month function errors when given invalid spec", {
  expect_error(in_month("Febr"))
  expect_error(in_month(13))
})
