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
#
# test_that("combined occuring functions work", {
#   my_birthday <- on_mday(12) %>% only_occuring(in_month("Jul"))
#   sisters_birthday <- on_mday(30) %>% only_occuring(in_month("Jul"))
#   our_birthdays <- my_birthday %>% also_occuring(sisters_birthday)
#
#   expect_true(test_date(dmy("12/07/1990"), our_birthdays))
#   expect_true(test_date(dmy("30/07/1992"), our_birthdays))
#   expect_false(test_date(dmy("11/07/1990"), our_birthdays))
#   expect_true(test_date(dmy("12/07/2019"), our_birthdays))
#   expect_true(test_date(dmy("30/07/2019"), our_birthdays))
#   expect_false(test_date(dmy("12/06/2019"), our_birthdays))
#   expect_false(test_date(dmy("11/06/2019"), our_birthdays))
# })
#
# test_that("not_occuring() function works on single element", {
#   on_thursday <- on_wday("Thu")
#   not_thursday <- not_occuring(on_thursday)
#
#   expect_true(test_date(dmy("12/07/1990"), on_thursday))
#   expect_false(test_date(dmy("12/07/1990"), not_thursday))
# })
#
# test_that("not_occuring() function works in nested structure", {
#   weekdays_first_mil_week <-
#     in_week(1) %>%
#     not_occuring(on_wday("Sat")) %>%
#     not_occuring(on_wday("Sun"))
#
#   expect_false(test_date(dmy("01/01/2000"), weekdays_first_mil_week))
#   expect_true(test_date(dmy("03/01/2000"), weekdays_first_mil_week))
#
#   scheduled_dates <-
#     schedule(weekdays_first_mil_week,
#              from = dmy("25/12/1999"),
#              to = dmy("15/01/2000"))
#
#   expect_equal(scheduled_dates, seq.Date(ymd("2000-01-03"),
#                                          ymd("2000-01-07"),
#                                          "1 day"))
# })
#
# test_that("not_occuring() function works in started nested structure", {
#   weekdays_first_mil_week <-
#     not_occuring(on_wday("Sat")) %>%
#     not_occuring(on_wday("Sun")) %>%
#     only_occuring(in_week(1))
#
#
#   expect_false(test_date(dmy("01/01/2000"), weekdays_first_mil_week))
#   expect_true(test_date(dmy("03/01/2000"), weekdays_first_mil_week))
#
#   scheduled_dates <-
#     schedule(weekdays_first_mil_week,
#              from = dmy("25/12/1999"),
#              to = dmy("15/01/2000"))
#
#   expect_equal(scheduled_dates, seq.Date(ymd("2000-01-03"),
#                                          ymd("2000-01-07"),
#                                          "1 day"))
# })
