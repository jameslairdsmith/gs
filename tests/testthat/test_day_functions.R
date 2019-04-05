context("test_day_functions")

library(lubridate)

test_that("on_yday function works", {

  first_day_of_year <- on_yday(1)

  expect_true(test_date(dmy("01/01/2000"), first_day_of_year))
  expect_false(test_date(dmy("02/01/2000"), first_day_of_year))
})

test_that("on_qday function works", {

  first_day_of_quarter <- on_qday(1)

  expect_true(test_date(dmy("01/01/2000"), first_day_of_quarter))
  expect_true(test_date(dmy("01/04/2000"), first_day_of_quarter))
  expect_false(test_date(dmy("02/04/2000"), first_day_of_quarter))
})

test_that("on_mday function works", {

  first_day_of_month <- on_mday(1)

  expect_true(test_date(dmy("01/01/2000"), first_day_of_month))
  expect_true(test_date(dmy("01/02/2000"), first_day_of_month))
  expect_true(test_date(dmy("01/04/2000"), first_day_of_month))
  expect_false(test_date(dmy("02/04/2000"), first_day_of_month))
  expect_false(test_date(dmy("02/01/2000"), first_day_of_month))
})

test_that("on_wday function works with number", {

  on_Thursday <- on_wday(5)

  expect_true(test_date(dmy("12/07/1990"), on_Thursday))
  expect_false(test_date(dmy("13/07/1990"), on_Thursday))
  expect_false(test_date(dmy("11/07/1990"), on_Thursday))
  expect_true(test_date(dmy("19/07/1990"), on_Thursday))
  expect_false(test_date(dmy("20/07/1990"), on_Thursday))
})

test_that("user can change week_start on_wday function", {

  on_Thursday <- on_wday(4, week_start = 1)

  expect_true(test_date(dmy("12/07/1990"), on_Thursday))
  expect_false(test_date(dmy("13/07/1990"), on_Thursday))
  expect_false(test_date(dmy("11/07/1990"), on_Thursday))
  expect_true(test_date(dmy("19/07/1990"), on_Thursday))
  expect_false(test_date(dmy("20/07/1990"), on_Thursday))
})

test_that("on_wday function works with abbreviated name of day", {

  on_Thursday <- on_wday("Thu")

  expect_true(test_date(dmy("12/07/1990"), on_Thursday))
  expect_false(test_date(dmy("13/07/1990"), on_Thursday))
  expect_false(test_date(dmy("11/07/1990"), on_Thursday))
  expect_true(test_date(dmy("19/07/1990"), on_Thursday))
  expect_false(test_date(dmy("20/07/1990"), on_Thursday))
})

test_that("on_wday function works with full name of day", {

  on_Thursday <- on_wday("Thursday")

  expect_true(test_date(dmy("12/07/1990"), on_Thursday))
  expect_false(test_date(dmy("13/07/1990"), on_Thursday))
  expect_false(test_date(dmy("11/07/1990"), on_Thursday))
  expect_true(test_date(dmy("19/07/1990"), on_Thursday))
  expect_false(test_date(dmy("20/07/1990"), on_Thursday))
})

test_that("on_wday function errors with invalid input", {
  expect_error(on_wday("Thurs"))
  expect_error(on_wday(8))
})

# test_that("on_wday function can override name check", {
#   expect_error(on_wday("Thurs", override_name_check = TRUE), NA)
#   expect_error(on_wday(8, override_name_check = TRUE), NA)
# })
