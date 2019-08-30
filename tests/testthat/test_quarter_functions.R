library(lubridate)

test_that("in_quarter() functions works", {

  in_q1 <- in_quarter(1)

  expect_true(happen(in_q1, dmy("01-01-2000")))
  expect_true(happen(in_q1, dmy("02-01-2000")))
  expect_true(happen(in_q1, dmy("01-02-2000")))
  expect_true(happen(in_q1, dmy("01-03-2000")))
  expect_false(happen(in_q1, dmy("01-04-2000")))
  expect_false(happen(in_q1, dmy("01-05-2000")))
  expect_false(happen(in_q1, dmy("30-06-2000")))
  expect_false(happen(in_q1, dmy("01-09-2000")))

  in_q2 <- in_quarter(2)

  expect_false(happen(in_q2, dmy("01-01-2000")))
  expect_false(happen(in_q2, dmy("02-01-2000")))
  expect_false(happen(in_q2, dmy("01-02-2000")))
  expect_false(happen(in_q2, dmy("01-03-2000")))
  expect_true(happen(in_q2, dmy("01-04-2000")))
  expect_true(happen(in_q2, dmy("01-05-2000")))
  expect_true(happen(in_q2, dmy("30-06-2000")))
  expect_false(happen(in_q2, dmy("01-09-2000")))

})

test_that("in_quarter() functions works with fiscal start specified", {

  in_q1 <- in_quarter(1, fiscal_start = 2)

  expect_false(happen(in_q1, dmy("01-01-2000")))
  expect_false(happen(in_q1, dmy("02-01-2000")))
  expect_true(happen(in_q1, dmy("01-02-2000")))
  expect_true(happen(in_q1, dmy("01-03-2000")))
  expect_true(happen(in_q1, dmy("01-04-2000")))
  expect_false(happen(in_q1, dmy("01-05-2000")))
  expect_false(happen(in_q1, dmy("30-06-2000")))
  expect_false(happen(in_q1, dmy("01-09-2000")))

  in_q2 <- in_quarter(2, fiscal_start = 2)

  expect_false(happen(in_q2, dmy("01-01-2000")))
  expect_false(happen(in_q2, dmy("02-01-2000")))
  expect_false(happen(in_q2, dmy("01-02-2000")))
  expect_false(happen(in_q2, dmy("01-03-2000")))
  expect_false(happen(in_q2, dmy("01-04-2000")))
  expect_true(happen(in_q2, dmy("01-05-2000")))
  expect_true(happen(in_q2, dmy("01-07-2000")))
  expect_true(happen(in_q2, dmy("30-06-2000")))
  expect_false(happen(in_q2, dmy("01-09-2000")))

})

test_that("in_quarter() functions works with mutiple inputs", {

  in_q1_or_q2 <- in_quarter(1, 2)

  expect_true(happen(in_q1_or_q2, dmy("01-01-2000")))
  expect_true(happen(in_q1_or_q2, dmy("02-01-2000")))
  expect_true(happen(in_q1_or_q2, dmy("01-02-2000")))
  expect_true(happen(in_q1_or_q2, dmy("01-03-2000")))
  expect_true(happen(in_q1_or_q2, dmy("01-04-2000")))
  expect_true(happen(in_q1_or_q2, dmy("01-05-2000")))
  expect_true(happen(in_q1_or_q2, dmy("30-06-2000")))
  expect_false(happen(in_q1_or_q2, dmy("01-09-2000")))
  expect_false(happen(in_q1_or_q2, dmy("01-08-2000")))
  expect_false(happen(in_q1_or_q2, dmy("31-10-2000")))

})

test_that("in_quarter() functions works with mutiple inputs", {

  in_q1_or_q2 <- in_quarter(1:2)

  expect_true(happen(in_q1_or_q2, dmy("01-01-2000")))
  expect_true(happen(in_q1_or_q2, dmy("02-01-2000")))
  expect_true(happen(in_q1_or_q2, dmy("01-02-2000")))
  expect_true(happen(in_q1_or_q2, dmy("01-03-2000")))
  expect_true(happen(in_q1_or_q2, dmy("01-04-2000")))
  expect_true(happen(in_q1_or_q2, dmy("01-05-2000")))
  expect_true(happen(in_q1_or_q2, dmy("30-06-2000")))
  expect_false(happen(in_q1_or_q2, dmy("01-09-2000")))
  expect_false(happen(in_q1_or_q2, dmy("01-08-2000")))
  expect_false(happen(in_q1_or_q2, dmy("31-10-2000")))

})

test_that("in_semester() functions works", {

  in_sem_1 <- in_semester(1)

  expect_true(happen(in_sem_1, dmy("01-01-2000")))
  expect_true(happen(in_sem_1, dmy("02-01-2000")))
  expect_true(happen(in_sem_1, dmy("01-02-2000")))
  expect_true(happen(in_sem_1, dmy("01-03-2000")))
  expect_true(happen(in_sem_1, dmy("01-04-2000")))
  expect_true(happen(in_sem_1, dmy("01-05-2000")))
  expect_true(happen(in_sem_1, dmy("30-06-2000")))
  expect_false(happen(in_sem_1, dmy("01-09-2000")))
  expect_false(happen(in_sem_1, dmy("01-10-2000")))
  expect_false(happen(in_sem_1, dmy("01-12-2000")))
  expect_false(happen(in_sem_1, dmy("31-12-2000")))

  in_sem_2 <- in_semester(2)

  expect_false(happen(in_sem_2, dmy("01-01-2000")))
  expect_false(happen(in_sem_2, dmy("02-01-2000")))
  expect_false(happen(in_sem_2, dmy("01-02-2000")))
  expect_false(happen(in_sem_2, dmy("01-03-2000")))
  expect_false(happen(in_sem_2, dmy("01-04-2000")))
  expect_false(happen(in_sem_2, dmy("01-05-2000")))
  expect_false(happen(in_sem_2, dmy("30-06-2000")))
  expect_true(happen(in_sem_2, dmy("01-09-2000")))
  expect_true(happen(in_sem_2, dmy("01-10-2000")))
  expect_true(happen(in_sem_2, dmy("01-12-2000")))
  expect_true(happen(in_sem_2, dmy("31-12-2000")))
})
