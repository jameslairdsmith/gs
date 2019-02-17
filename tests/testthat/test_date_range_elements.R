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
