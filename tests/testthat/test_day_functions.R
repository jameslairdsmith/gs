context("test-day_functions")

library(lubridate)

test_that("on_yday function works", {

  first_day_of_year <- on_yday(1)

  expect_true(test_date(first_day_of_year, dmy("01/01/2000")))
})
