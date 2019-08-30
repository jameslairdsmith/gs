context("test_day_functions")

library(lubridate)

test_that("on_yday function works", {

  first_day_of_year <- on_yday(1)

  expect_true(test_date(dmy("01/01/2000"), first_day_of_year))
  expect_false(test_date(dmy("02/01/2000"), first_day_of_year))
})

test_that("on_yday function works with multiple inputs", {

  first_or_second_day_of_year <- on_yday(1, 2)

  expect_true(test_date(dmy("01/01/2000"), first_or_second_day_of_year))
  expect_true(test_date(dmy("02/01/2000"), first_or_second_day_of_year))
  expect_false(test_date(dmy("03/01/2000"), first_or_second_day_of_year))
})

test_that("on_yday function errors on invalid input", {

  expect_error(on_yday(367))
  expect_error(on_yday(3678))
  expect_error(on_yday(0))
  expect_error(on_yday(1.5))
  expect_error(on_yday(-1))
  expect_error(on_yday(-365))
})

test_that("on_qday function works", {

  first_day_of_quarter <- on_qday(1)

  expect_true(test_date(dmy("01/01/2000"), first_day_of_quarter))
  expect_true(test_date(dmy("01/04/2000"), first_day_of_quarter))
  expect_false(test_date(dmy("02/04/2000"), first_day_of_quarter))
})

test_that("on_qday function errors on invalid input", {

  expect_error(on_qday(93))
  expect_error(on_qday(0))
  expect_error(on_qday(1.5))
  expect_error(on_qday(-1))
  expect_error(on_qday(-90))
})

test_that("on_qday function works with multiple inputs", {

  first_or_second_day_of_quarter <- on_qday(1, 2)

  expect_true(test_date(dmy("01/01/2000"), first_or_second_day_of_quarter))
  expect_true(test_date(dmy("02/01/2000"), first_or_second_day_of_quarter))
  expect_false(test_date(dmy("03/01/2000"), first_or_second_day_of_quarter))
  expect_true(test_date(dmy("01/04/2000"), first_or_second_day_of_quarter))
  expect_true(test_date(dmy("02/04/2000"), first_or_second_day_of_quarter))
  expect_false(test_date(dmy("03/04/2000"), first_or_second_day_of_quarter))
})

test_that("on_mday function works", {

  first_day_of_month <- on_mday(1)

  expect_true(test_date(dmy("01/01/2000"), first_day_of_month))
  expect_true(test_date(dmy("01/02/2000"), first_day_of_month))
  expect_true(test_date(dmy("01/04/2000"), first_day_of_month))
  expect_false(test_date(dmy("02/04/2000"), first_day_of_month))
  expect_false(test_date(dmy("02/01/2000"), first_day_of_month))
})

test_that("on_mday function errors with invalid input", {

  expect_error(on_mday(32))
  expect_error(on_mday(0))
  expect_error(on_mday(-1))
  expect_error(on_mday(1.5))
  expect_error(on_mday(-30))
})

test_that("on_mday function works with multiple inputs", {

  first_or_second_day_of_month <- on_mday(1, 2)

  expect_true(test_date(dmy("01/01/2000"), first_or_second_day_of_month))
  expect_true(test_date(dmy("01/02/2000"), first_or_second_day_of_month))
  expect_true(test_date(dmy("01/04/2000"), first_or_second_day_of_month))
  expect_true(test_date(dmy("02/04/2000"), first_or_second_day_of_month))
  expect_true(test_date(dmy("02/01/2000"), first_or_second_day_of_month))
  expect_false(test_date(dmy("03/04/2000"), first_or_second_day_of_month))
  expect_false(test_date(dmy("03/01/2000"), first_or_second_day_of_month))
})


test_that("on_wday function works with number", {

  on_Thursday <- on_wday(5)

  expect_true(test_date(dmy("12/07/1990"), on_Thursday))
  expect_false(test_date(dmy("13/07/1990"), on_Thursday))
  expect_false(test_date(dmy("11/07/1990"), on_Thursday))
  expect_true(test_date(dmy("19/07/1990"), on_Thursday))
  expect_false(test_date(dmy("20/07/1990"), on_Thursday))
})

test_that("on_wday function works with two numbers", {

  on_Thursday_or_Friday <- on_wday(5, 6)

  expect_true(test_date(dmy("12/07/1990"), on_Thursday_or_Friday))
  expect_true(test_date(dmy("13/07/1990"), on_Thursday_or_Friday))
  expect_false(test_date(dmy("14/07/1990"), on_Thursday_or_Friday))
  expect_false(test_date(dmy("11/07/1990"), on_Thursday_or_Friday))
  expect_true(test_date(dmy("19/07/1990"), on_Thursday_or_Friday))
  expect_true(test_date(dmy("20/07/1990"), on_Thursday_or_Friday))
  expect_false(test_date(dmy("21/07/1990"), on_Thursday_or_Friday))
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

test_that("on_wday function works with abbreviated name of two days", {

  on_Thursday_or_Friday <- on_wday("Thu", "Fri")

  expect_true(test_date(dmy("12/07/1990"), on_Thursday_or_Friday))
  expect_true(test_date(dmy("13/07/1990"), on_Thursday_or_Friday))
  expect_false(test_date(dmy("14/07/1990"), on_Thursday_or_Friday))
  expect_false(test_date(dmy("11/07/1990"), on_Thursday_or_Friday))
  expect_true(test_date(dmy("19/07/1990"), on_Thursday_or_Friday))
  expect_true(test_date(dmy("20/07/1990"), on_Thursday_or_Friday))
  expect_false(test_date(dmy("21/07/1990"), on_Thursday_or_Friday))
})

test_that("on_wday function works with full name of day", {

  on_Thursday <- on_wday("Thursday")

  expect_true(test_date(dmy("12/07/1990"), on_Thursday))
  expect_false(test_date(dmy("13/07/1990"), on_Thursday))
  expect_false(test_date(dmy("11/07/1990"), on_Thursday))
  expect_true(test_date(dmy("19/07/1990"), on_Thursday))
  expect_false(test_date(dmy("20/07/1990"), on_Thursday))
})

test_that("on_wday function works with full name of two days", {

  on_Thursday_or_Friday <- on_wday("Thursday", "Friday")

  expect_true(test_date(dmy("12/07/1990"), on_Thursday_or_Friday))
  expect_true(test_date(dmy("13/07/1990"), on_Thursday_or_Friday))
  expect_false(test_date(dmy("14/07/1990"), on_Thursday_or_Friday))
  expect_false(test_date(dmy("11/07/1990"), on_Thursday_or_Friday))
  expect_true(test_date(dmy("19/07/1990"), on_Thursday_or_Friday))
  expect_true(test_date(dmy("20/07/1990"), on_Thursday_or_Friday))
  expect_false(test_date(dmy("21/07/1990"), on_Thursday_or_Friday))
})

test_that("on_wday function works with multiple mixed day specs", {

  on_Thursday_or_Friday_or_Sat <- on_wday("Thursday", 6, "Sat")

  expect_true(test_date(dmy("12/07/1990"), on_Thursday_or_Friday_or_Sat))
  expect_true(test_date(dmy("13/07/1990"), on_Thursday_or_Friday_or_Sat))
  expect_true(test_date(dmy("14/07/1990"), on_Thursday_or_Friday_or_Sat))
  expect_false(test_date(dmy("15/07/1990"), on_Thursday_or_Friday_or_Sat))
  expect_false(test_date(dmy("11/07/1990"), on_Thursday_or_Friday_or_Sat))
  expect_true(test_date(dmy("19/07/1990"), on_Thursday_or_Friday_or_Sat))
  expect_true(test_date(dmy("20/07/1990"), on_Thursday_or_Friday_or_Sat))
  expect_true(test_date(dmy("21/07/1990"), on_Thursday_or_Friday_or_Sat))
  expect_false(test_date(dmy("22/07/1990"), on_Thursday_or_Friday_or_Sat))
})

test_that("on_wday function errors with invalid input", {
  expect_error(on_wday("Thurs"))
  expect_error(on_wday(8))
})

test_that("on_weekend function works", {
  expect_false(test_date(dmy("20/07/1990"), on_weekend()))
  expect_true(test_date(dmy("21/07/1990"), on_weekend()))
  expect_true(test_date(dmy("22/07/1990"), on_weekend()))
})

test_that("on_weekday function works", {
  expect_true(happen(on_weekday(), dmy("20/07/1990")))
  expect_false(happen(on_weekday(), dmy("21/07/1990")))
  expect_false(happen(on_weekday(), dmy("22/07/1990")))
})
