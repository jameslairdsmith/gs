context("test_month_functions")

test_that("in_month function work", {

  in_july <- in_month("Jul")

  expect_true(test_date(dmy("12/07/1990"), in_july))
  expect_true(test_date(dmy("13/07/1990"), in_july))
  expect_false(test_date(dmy("12/06/1990"), in_july))
  expect_false(test_date(dmy("13/06/1990"), in_july))
})

test_that("in_month function works with numbers", {

  in_july <- in_month(7)

  expect_true(test_date(dmy("12/07/1990"), in_july))
  expect_true(test_date(dmy("13/07/1990"), in_july))
  expect_false(test_date(dmy("12/06/1990"), in_july))
  expect_false(test_date(dmy("13/06/1990"), in_july))
})

test_that("in_month function works with two numbers", {

  in_july_or_aug <- in_month(7, 8)

  expect_true(test_date(dmy("12/07/1990"), in_july_or_aug))
  expect_true(test_date(dmy("12/08/1990"), in_july_or_aug))
  expect_true(test_date(dmy("13/07/1990"), in_july_or_aug))
  expect_true(test_date(dmy("13/08/1990"), in_july_or_aug))
  expect_false(test_date(dmy("12/06/1990"), in_july_or_aug))
  expect_false(test_date(dmy("13/06/1990"), in_july_or_aug))

  result <- schedule(in_july_or_aug, during = 2000)
  expected_result <- seq.Date(from = dmy("01/07/2000"),
                               to = dmy("31/08/2000"),
                               by = "1 day")

  expect_equal(result, expected_result)

})

test_that("in_month function works with two numbers, one in Feb", {

  in_feb_march <- in_month(2, 3)

  result <- schedule(in_feb_march, during = 2001)
  expected_result <- seq.Date(from = dmy("01/02/2001"),
                              to = dmy("31/03/2001"),
                              by = "1 day")

  expect_equal(result, expected_result)

})

test_that("in_month function works with string numbers", {

  in_july <- in_month("7")

  expect_true(test_date(dmy("12/07/1990"), in_july))
  expect_true(test_date(dmy("13/07/1990"), in_july))
  expect_false(test_date(dmy("12/06/1990"), in_july))
  expect_false(test_date(dmy("13/06/1990"), in_july))
})

test_that("in_month function works with two string numbers", {

  in_july_or_aug <- in_month("7", "8")

  expect_true(test_date(dmy("12/07/1990"), in_july_or_aug))
  expect_true(test_date(dmy("12/08/1990"), in_july_or_aug))
  expect_true(test_date(dmy("13/07/1990"), in_july_or_aug))
  expect_true(test_date(dmy("13/08/1990"), in_july_or_aug))
  expect_false(test_date(dmy("12/06/1990"), in_july_or_aug))
  expect_false(test_date(dmy("13/06/1990"), in_july_or_aug))
})

test_that("in_month function errors when given invalid spec", {
  expect_error(in_month("Febr"))
  expect_error(in_month(13))
})

test_that("in_month function works with two months", {

  expected_dates <- seq.Date(from = dmy("01/01/2000"),
                             to = dmy("29/02/2000"),
                             by = "1 day")

  Jan_and_Feb <- in_month("Jan", "Feb")

  expect_equal(expected_dates, schedule(Jan_and_Feb, during = 2000))
})

test_that("in_month function works with mutliple months", {

  expected_dates <- c(seq.Date(from = dmy("01/01/2000"),
                               to = dmy("29/02/2000"),
                               by = "1 day"),
                      seq.Date(from = dmy("01/12/2000"),
                               to = dmy("31/12/2000"),
                               by = "1 day"))

  Jan_Feb_and_Dec <- in_month("Jan", "Feb", "Dec")

  expect_equal(schedule(Jan_Feb_and_Dec, during = 2000), expected_dates)
})

test_that("in_month function works with mutliple months specified as integers", {

  expected_dates <- c(seq.Date(from = dmy("01/01/2000"),
                               to = dmy("29/02/2000"),
                               by = "1 day"),
                      seq.Date(from = dmy("01/12/2000"),
                               to = dmy("31/12/2000"),
                               by = "1 day"))

  Jan_Feb_and_Dec <- in_month(1, 2, 12)

  expect_equal(schedule(Jan_Feb_and_Dec, during = 2000), expected_dates)
})

test_that("in_month function works with mixed month specifications", {

  expected_dates <- c(seq.Date(from = dmy("01/01/2000"),
                               to = dmy("29/02/2000"),
                               by = "1 day"),
                      seq.Date(from = dmy("01/12/2000"),
                               to = dmy("31/12/2000"),
                               by = "1 day"))

  Jan_Feb_and_Dec <- in_month(1, "Feb", "December")

  expect_equal(schedule(Jan_Feb_and_Dec, during = 2000), expected_dates)
})
