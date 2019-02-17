context("test_date_functions")

test_that("on_date function works", {

  my_birthday <- lubridate::dmy("12/07/1990")

  expect_true(test_date(my_birthday, on_date(my_birthday)))
})
