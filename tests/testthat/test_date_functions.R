context("test_date_functions")

library(lubridate)

test_that("on_date() function works", {

  my_birthday <- dmy("12/07/1990")

  expect_true(test_date(my_birthday, on_date(my_birthday)))
  expect_false(test_date(dmy("12/07/2019"), on_date(my_birthday)))
})

test_that("after() function works", {

  my_birthday <- on_mday(12) %>% only_occuring(in_month("Jul"))

  expect_true(test_date(dmy("20/07/2019"), after(my_birthday, within_given = lubridate::month)))
  expect_false(test_date(dmy("07/07/2019"), after(my_birthday, within_given = lubridate::month)))
})

test_that("after() function works with string for within_given", {

  my_birthday <- on_mday(12) %>% only_occuring(in_month("Jul"))

  expect_true(test_date(dmy("20/07/2019"), after(my_birthday, within_given = "month")))
  expect_false(test_date(dmy("07/07/2019"), after(my_birthday, within_given = "month")))
})

test_that("after() function works on a vector", {

  my_birthday <- on_mday(12) %>% only_occuring(in_month("Jul"))

  expected_result <- c(F, F, F, T, T)
  days_around_birthday <- seq.Date(from = dmy("10/07/1990"), to = dmy("14/07/1990"), by = "1 day")

  after_birthday <- after(my_birthday, within_given = "month")

  expect_equal(test_date(days_around_birthday, after_birthday), expected_result)
})

test_that("after() function works on a long vector", {

  my_birthday <- on_mday(12) %>% only_occuring(in_month("Jul"))

  expected_result <- seq.Date(from = dmy("13/07/1990"), to = dmy("31/07/1990"), by = "1 day")
  days_in_1990 <- seq.Date(from = dmy("01/01/1990"), to = dmy("31/12/1990"), by = "1 day")

  after_birthday <- after(my_birthday, within_given = "month")

  expect_equal(days_in_1990[test_date(days_in_1990, after_birthday)], expected_result)
})

test_that("before() function works", {

  my_birthday <- on_mday(12) %>% only_occuring(in_month("Jul"))

  expect_false(test_date(dmy("20/07/2019"), before(my_birthday, within_given = lubridate::month)))
  expect_true(test_date(dmy("07/07/2019"), before(my_birthday, within_given = lubridate::month)))
})

test_that("before() function works on a vector", {

  my_birthday <- on_mday(12) %>% only_occuring(in_month("Jul"))

  expected_result <- c(T, T, F, F, F)
  days_around_birthday <- seq.Date(from = dmy("10/07/1990"), to = dmy("14/07/1990"), by = "1 day")

  before_birthday <- before(my_birthday, within_given = "month")

  expect_equal(test_date(days_around_birthday, before_birthday), expected_result)
})

test_that("before() function works on a long vector", {

  my_birthday <- on_mday(12) %>% only_occuring(in_month("Jul"))

  expected_result <- seq.Date(from = dmy("01/07/1990"), to = dmy("11/07/1990"), by = "1 day")
  days_in_1990 <- seq.Date(from = dmy("01/01/1990"), to = dmy("31/12/1990"), by = "1 day")

  before_birthday <- before(my_birthday, within_given = "month")

  expect_equal(days_in_1990[test_date(days_in_1990, before_birthday)], expected_result)
})

test_that("before() function works with string value for within_given", {

  my_birthday <- on_mday(12) %>% only_occuring(in_month("Jul"))

  expect_false(test_date(dmy("20/07/2019"), before(my_birthday, within_given = "month")))
  expect_true(test_date(dmy("07/07/2019"), before(my_birthday, within_given = "month")))
})


