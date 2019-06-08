context("test_helper_functions_work")

test_that("schedule_annually works with start date", {
  expected_result <- seq.Date(from = dmy("01/01/2000"), to = dmy("01-01-2005"), by = "1 year")
  result <- schedule_annually(starting = dmy("01/01/2000"), years = 5)

  expect_identical(expected_result, result)
})

test_that("schedule_annually works with start date and leap year", {

  expected_result <- c(ymd("1992-02-29"),
                       ymd("1993-02-28"),
                       ymd("1994-02-28"),
                       ymd("1995-02-28"),
                       ymd("1996-02-29"),
                       ymd("1997-02-28"))

  result <- schedule_annually(starting = dmy("29/02/1992"), years = 5)

  expect_identical(expected_result, result)
})

test_that("schedule_annually can exclude start value", {

  expected_result <- c(ymd("1993-02-28"),
                       ymd("1994-02-28"),
                       ymd("1995-02-28"),
                       ymd("1996-02-29"),
                       ymd("1997-02-28"))

  result <- schedule_annually(starting = dmy("29/02/1992"), years = 5, inclusive = F)

  expect_identical(expected_result, result)
})

test_that("schedule_annually works with end date", {

  expected_result <- seq.Date(from = dmy("01/01/1987"), to = dmy("01-01-1992"), by = "1 year")
  result <- schedule_annually(ending = dmy("01/01/1992"), years = 5)

  expect_identical(expected_result, result)
})

test_that("schedule_annually can exclude end date", {

  expected_result <- seq.Date(from = dmy("01/01/1987"), to = dmy("01-01-1991"), by = "1 year")
  result <- schedule_annually(ending = dmy("01/01/1992"), years = 5, inclusive = F)

  expect_identical(expected_result, result)
})

test_that("schedule_annually works with end date and leap year", {

  expected_result <- c(ymd("1987-02-28"),
                       ymd("1988-02-29"),
                       ymd("1989-02-28"),
                       ymd("1990-02-28"),
                       ymd("1991-02-28"),
                       ymd("1992-02-29"))

  result <- schedule_annually(ending = dmy("29/02/1992"), years = 5)

  expect_identical(expected_result, result)
})
