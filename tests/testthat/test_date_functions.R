context("test_date_functions")

library(lubridate)

test_that("on_date function works", {

  my_birthday <- dmy("12/07/1990")

  expect_true(test_date(my_birthday, on_date(my_birthday)))
  expect_false(test_date(dmy("12/07/2019"), on_date(my_birthday)))
})
