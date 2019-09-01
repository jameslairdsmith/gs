library(lubridate)


test_that("on_every() function works with months", {

  produced_result <-
    on_every(months, dmy("28/02/2001"), inclusive = T, backdated = T) %>%
    schedule_days(during = 2001)

  expected_result <- dmy("28/02/2001") %m+% months(-1:10)

  expect_equal(produced_result, expected_result)

})

test_that("on_every() function works with months in leap year", {

  produced_result <-
    on_every(months, dmy("29/02/2000"), inclusive = T, backdated = T) %>%
    schedule_days(during = 2000)

  expected_result <- dmy("29/02/2000") %m+% months(-1:10)

  expect_equal(produced_result, expected_result)

})

test_that("on_every() function works with months of 30th", {

  produced_result <-
    on_every(months, dmy("30/01/2000"), inclusive = T, backdated = T) %>%
    schedule_days(during = 2000)

  expected_result <- dmy("30/01/2000") %m+% months(0:11)

  expect_equal(produced_result, expected_result)

})

test_that("on_every() function works with last days of month", {

  produced_result <-
    on_every(months, dmy("31/01/2000"), inclusive = T, backdated = T) %>%
    schedule_days(during = 2000)

  expected_result <- dmy("31/01/2000") %m+% months(0:11)

  expect_equal(produced_result, expected_result)

})

test_that("on_every() function works with years", {

  produced_result <-
    on_every(years, dmy("28/02/2001"), inclusive = T, backdated = T) %>%
    schedule_days(from = 2001, to = 2007)

  expected_result <- dmy("28/02/2001") %m+% years(0:6)

  expect_equal(produced_result, expected_result)

})

test_that("on_every() function works with leap years", {

  produced_result <-
    on_every(years, dmy("29/02/2000"), inclusive = T, backdated = T) %>%
    schedule_days(from = 2000, to = 2006)

  expected_result <- dmy("29/02/2000") %m+% years(0:6)

  expect_equal(produced_result, expected_result)

})

test_that("on_every() function works with days", {

  produced_result <-
    on_every(days, dmy("28/02/2001"), inclusive = T, backdated = F) %>%
    schedule_days(from = 2001, to = 2001)

  expected_result <- seq.Date(from = dmy("28/02/2001"),
                              to = dmy("31/12/2001"),
                              by = "1 day")

  expect_equal(produced_result, expected_result)

})


test_that("on_every() function works with weeks", {

  produced_result <-
    on_every(weeks, dmy("28/02/2001"), inclusive = T, backdated = F) %>%
    schedule_days(from = 2001, to = 2001)

  expected_result <- seq.Date(from = dmy("28/02/2001"),
                              to = dmy("31/12/2001"),
                              by = "1 week")

  expect_equal(produced_result, expected_result)

})

test_that("on_every() function works with weeks in leap year", {

  produced_result <-
    on_every(weeks, dmy("29/02/2000"), inclusive = T, backdated = F) %>%
    schedule_days(from = 2000, to = 2000)

  expected_result <- seq.Date(from = dmy("29/02/2000"),
                              to = dmy("31/12/2000"),
                              by = "1 week")

  expect_equal(produced_result, expected_result)

})
