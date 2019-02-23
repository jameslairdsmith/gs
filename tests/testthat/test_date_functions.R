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

test_that("before() function works", {

  my_birthday <- on_mday(12) %>% only_occuring(in_month("Jul"))

  expect_false(test_date(dmy("20/07/2019"), before(my_birthday, within_given = lubridate::month)))
  expect_true(test_date(dmy("07/07/2019"), before(my_birthday, within_given = lubridate::month)))
})


