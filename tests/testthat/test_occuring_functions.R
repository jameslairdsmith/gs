context("test_occuring_functions")

test_that("only_occuring function works", {
  is_my_birthday <- on_mday(12) %>% only_occuring(in_month("Jul"))

  expect_true(test_date(dmy("12/07/1990"), is_my_birthday))
  expect_false(test_date(dmy("13/07/1990"), is_my_birthday))
  expect_false(test_date(dmy("11/07/1990"), is_my_birthday))
  expect_true(test_date(dmy("12/07/2019"), is_my_birthday))
})

test_that("also_occuring function works", {
  is_my_birthday <- on_mday(12) %>% also_occuring(in_month("Jul"))

  expect_true(test_date(dmy("12/07/1990"), is_my_birthday))
  expect_true(test_date(dmy("13/07/1990"), is_my_birthday))
  expect_true(test_date(dmy("11/07/1990"), is_my_birthday))
  expect_true(test_date(dmy("12/07/2019"), is_my_birthday))
  expect_true(test_date(dmy("12/06/2019"), is_my_birthday))
  expect_false(test_date(dmy("11/06/2019"), is_my_birthday))
})

test_that("combined occuring functions work", {
  my_birthday <- on_mday(12) %>% only_occuring(in_month("Jul"))
  sisters_birthday <- on_mday(30) %>% only_occuring(in_month("Jul"))
  our_birthdays <- my_birthday %>% also_occuring(sisters_birthday)

  expect_true(test_date(dmy("12/07/1990"), our_birthdays))
  expect_true(test_date(dmy("30/07/1992"), our_birthdays))
  expect_false(test_date(dmy("11/07/1990"), our_birthdays))
  expect_true(test_date(dmy("12/07/2019"), our_birthdays))
  expect_true(test_date(dmy("30/07/2019"), our_birthdays))
  expect_false(test_date(dmy("12/06/2019"), our_birthdays))
  expect_false(test_date(dmy("11/06/2019"), our_birthdays))
})
