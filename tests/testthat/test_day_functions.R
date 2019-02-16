context("test_day_functions")

library(lubridate)

test_that("on_yday function works", {

  first_day_of_year <- on_yday(1)

  expect_true(test_date(dmy("01/01/2000"), first_day_of_year))
})
