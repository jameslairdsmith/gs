context("test_occuring_functions")

test_that("only_occuring function works", {
  is_my_birthday <- on_mday(12) %>% only_occuring(in_month("Jul"))

  expect_true(test_date(dmy("12/07/1990"), is_my_birthday))
  expect_false(test_date(dmy("13/07/1990"), is_my_birthday))
  expect_false(test_date(dmy("11/07/1990"), is_my_birthday))
  expect_true(test_date(dmy("12/07/2019"), is_my_birthday))
})
