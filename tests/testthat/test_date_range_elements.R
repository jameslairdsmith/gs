context("test_date_range_elements")

test_that("`on_nth` function works", {

  Thursday <- on_wday("Thu")
  second_thursday_of_month <-
    on_nth(2, Thursday, within_given = lubridate::month)
  first_thursday_of_month <-
    on_nth(1, Thursday, within_given = lubridate::month)
  last_thursday_of_month <-
    on_nth(-1, Thursday, within_given = lubridate::month)
  second_last_thursday_of_month <-
    on_nth(-2, Thursday, within_given = lubridate::month)

  expect_true(test_date(dmy("12/07/1990"), second_thursday_of_month))
  expect_false(test_date(dmy("05/07/1990"), second_thursday_of_month))
  expect_false(test_date(dmy("11/07/1990"), second_thursday_of_month))
  expect_true(test_date(dmy("05/07/1990"), first_thursday_of_month))
  expect_false(test_date(dmy("12/07/1990"), first_thursday_of_month))
  expect_true(test_date(dmy("26/07/1990"), last_thursday_of_month))
  expect_true(test_date(dmy("19/07/1990"), second_last_thursday_of_month))
  expect_false(test_date(dmy("12/07/1990"), second_last_thursday_of_month))

})

test_that("`on_nth` function works with string value for within_given", {

  Thursday <- on_wday("Thu")
  second_thursday_of_month <-
    on_nth(2, Thursday, within_given = "month")
  first_thursday_of_month <-
    on_nth(1, Thursday, within_given = "month")
  last_thursday_of_month <-
    on_nth(-1, Thursday, within_given = "month")
  second_last_thursday_of_month <-
    on_nth(-2, Thursday, within_given = "month")

  expect_true(test_date(dmy("12/07/1990"), second_thursday_of_month))
  expect_false(test_date(dmy("05/07/1990"), second_thursday_of_month))
  expect_false(test_date(dmy("11/07/1990"), second_thursday_of_month))
  expect_true(test_date(dmy("05/07/1990"), first_thursday_of_month))
  expect_false(test_date(dmy("12/07/1990"), first_thursday_of_month))
  expect_true(test_date(dmy("26/07/1990"), last_thursday_of_month))
  expect_true(test_date(dmy("19/07/1990"), second_last_thursday_of_month))
  expect_false(test_date(dmy("12/07/1990"), second_last_thursday_of_month))

})

test_that("`on_nth` convenience functions work", {

  Thursday <- on_wday("Thu")
  second_thursday_of_month <-
    on_second(Thursday, within_given = lubridate::month)
  first_thursday_of_month <-
    on_first(Thursday, within_given = lubridate::month)
  last_thursday_of_month <-
    on_last(Thursday, within_given = lubridate::month)

  expect_true(test_date(dmy("12/07/1990"), second_thursday_of_month))
  expect_false(test_date(dmy("05/07/1990"), second_thursday_of_month))
  expect_false(test_date(dmy("11/07/1990"), second_thursday_of_month))
  expect_true(test_date(dmy("05/07/1990"), first_thursday_of_month))
  expect_false(test_date(dmy("12/07/1990"), first_thursday_of_month))
  expect_true(test_date(dmy("26/07/1990"), last_thursday_of_month))
})

test_that("`on_nth` function works as part of a schedule", {

  Thursday <- on_wday("Thu")
  second_thursday_of_july <-
    on_nth(2, Thursday, within_given = lubridate::month) %>%
    only_occuring(in_month("Jul"))

  expect_true(test_date(dmy("12/07/1990"), second_thursday_of_july))
  expect_false(test_date(dmy("05/07/1990"), second_thursday_of_july))
  expect_false(test_date(dmy("11/07/1990"), second_thursday_of_july))
})

test_that("`on_nth` function works on a vector", {

  Thursday <- on_wday("Thu")
  second_thursday_of_month <-
    on_nth(2, Thursday, within_given = lubridate::month)

  date_vector <- c(dmy("12/07/1990"), dmy("13/07/1990"))

  expect_equal(test_date(date_vector, second_thursday_of_month), c(TRUE, FALSE))

})
