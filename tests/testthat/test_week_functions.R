context("test_week_functions")

test_that("in_week functions works", {
  first_week_of_year <- in_week(1)

  expect_true(test_date(dmy("01/01/2000"), first_week_of_year))
  expect_true(test_date(dmy("02/01/2000"), first_week_of_year))
  expect_true(test_date(dmy("01/01/1990"), first_week_of_year))
  expect_false(test_date(dmy("31/12/1990"), first_week_of_year))

  twenty_eight_week_of_year <- in_week(28)

  expect_true(test_date(dmy("12/07/1990"), twenty_eight_week_of_year))
  expect_false(test_date(dmy("20/07/1990"), twenty_eight_week_of_year))
})

test_that("in_isoweek functions works", {
  first_isoweek_of_year <- in_isoweek(1)
  last_isoweek_of_year <- in_isoweek(52)

  expect_true(test_date(dmy("01/01/2000"), last_isoweek_of_year))
  expect_true(test_date(dmy("03/01/2000"), first_isoweek_of_year))
  expect_false(test_date(dmy("02/01/2000"), first_isoweek_of_year))
  expect_true(test_date(dmy("01/01/1990"), first_isoweek_of_year))
  expect_true(test_date(dmy("31/12/1990"), first_isoweek_of_year))
})

in_epiweek <- function(x, ...){
  make_element(x, lubridate::epiweek, ...)
}
