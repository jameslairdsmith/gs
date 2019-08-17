library(lubridate)

test_that("make_period_seq() function works with days", {

  start_date <- as.Date("2000-02-15")
  end_date <- as.Date("2000-03-25")

  my_seq <- make_period_seq(start_date,
                            end_date,
                            period_unit = "day",
                            period_n = 1)

  expected_day_result <- seq.Date(start_date,
                                  end_date,
                                  by = "1 day")

  expect_equal(my_seq, expected_day_result)
})

test_that("make_period_seq() function works with 2 days", {

  start_date <- as.Date("2000-02-15")
  end_date <- as.Date("2000-03-25")

  my_seq <- make_period_seq(start_date,
                            end_date,
                            period_unit = "day",
                            period_n = 2)

  expected_day_result <- seq.Date(start_date,
                                  end_date,
                                  by = "2 day")

  expect_equal(my_seq, expected_day_result)
})

test_that("make_period_seq() function works with days in non-leap years", {

  start_date <- as.Date("2001-02-15")
  end_date <- as.Date("2001-03-25")

  my_seq <- make_period_seq(start_date,
                            end_date,
                            period_unit = "day",
                            period_n = 1)

  expected_day_result <- seq.Date(start_date,
                                  end_date,
                                  by = "1 day")

  expect_equal(my_seq, expected_day_result)
})

test_that("make_period_seq() function works with hours", {

  start_date <- as_datetime(as.Date("2000-02-15"))
  end_date <- as_datetime(as.Date("2000-03-25"))

  my_seq <- make_period_seq(start_date,
                            end_date,
                            period_unit = "hour",
                            period_n = 1)

  expect_equal(head(my_seq, 1),
               make_datetime(year = 2000, month = 2, day = 15))

  expect_equal(tail(my_seq, 1),
               make_datetime(year = 2000, month = 3, day = 25, hour = 23))

})

test_that("make_period_seq() function works with minutes", {

  start_date <- as_datetime(as.Date("2000-02-15"))
  end_date <- as_datetime(as.Date("2000-03-25"))

  my_seq <- make_period_seq(start_date,
                            end_date,
                            period_unit = "minute",
                            period_n = 1)

  expect_equal(head(my_seq, 1),
               make_datetime(year = 2000, month = 2, day = 15))

  expect_equal(tail(my_seq, 1),
               make_datetime(year = 2000,
                             month = 3,
                             day = 25,
                             hour = 23,
                             min = 59))

})

test_that("make_period_seq() function works with seconds", {

  start_date <- as_datetime(as.Date("2000-02-15"))
  end_date <- as_datetime(as.Date("2000-03-25"))

  my_seq <- make_period_seq(start_date,
                            end_date,
                            period_unit = "second",
                            period_n = 1)

  expect_equal(head(my_seq, 1),
               make_datetime(year = 2000, month = 2, day = 15))

  expect_equal(tail(my_seq, 1),
               make_datetime(year = 2000,
                             month = 3,
                             day = 25,
                             hour = 23,
                             min = 59,
                             sec = 59))

})
