test_that("in_year() function works", {
  in_1999 <- in_year(1999)

  expect_true(happen(in_1999, dmy("01/01/1999")))
  expect_true(happen(in_1999, dmy("02/01/1999")))
  expect_true(happen(in_1999, dmy("01/02/1999")))
  expect_true(happen(in_1999, dmy("31/12/1999")))
  expect_false(happen(in_1999, dmy("31/12/1998")))
  expect_false(happen(in_1999, dmy("01/01/2000")))

  my_result <- schedule(in_1999, from = 1995, to = 2005)

  expected_result <- seq.Date(from = dmy("01/01/1999"),
                              to = dmy("31/12/1999"),
                              by = "1 day")

  expect_equal(my_result, expected_result)
})

test_that("in_year() function works in leap year", {
  in_2000 <- in_year(2000)

  expect_true(happen(in_2000, dmy("01/01/2000")))
  expect_true(happen(in_2000, dmy("02/01/2000")))
  expect_true(happen(in_2000, dmy("01/02/2000")))
  expect_true(happen(in_2000, dmy("29/02/2000")))
  expect_true(happen(in_2000, dmy("31/12/2000")))
  expect_false(happen(in_2000, dmy("31/12/1999")))
  expect_false(happen(in_2000, dmy("01/01/2001")))

  my_result <- schedule(in_2000, from = 1995, to = 2005)

  expected_result <- seq.Date(from = dmy("01/01/2000"),
                              to = dmy("31/12/2000"),
                              by = "1 day")

  expect_equal(my_result, expected_result)
})

test_that("in_year() function works with multiple inputs", {
  in_1999_or_2000 <- in_year(1999, 2000)

  expect_true(happen(in_1999_or_2000, dmy("01/01/1999")))
  expect_true(happen(in_1999_or_2000, dmy("02/01/1999")))
  expect_true(happen(in_1999_or_2000, dmy("31/12/1999")))
  expect_true(happen(in_1999_or_2000, dmy("01/01/2000")))
  expect_true(happen(in_1999_or_2000, dmy("02/01/2000")))
  expect_true(happen(in_1999_or_2000, dmy("29/02/2000")))
  expect_true(happen(in_1999_or_2000, dmy("31/12/2000")))
  expect_false(happen(in_1999_or_2000, dmy("31/12/1998")))
  expect_false(happen(in_1999_or_2000, dmy("01/01/2001")))

  my_result <- schedule(in_1999_or_2000, from = 1995, to = 2005)

  expected_result <- seq.Date(from = dmy("01/01/1999"),
                              to = dmy("31/12/2000"),
                              by = "1 day")

  expect_equal(my_result, expected_result)
})

test_that("in_year() function carries attributes", {
  in_1999_or_2000 <- in_year(1999, 2000)

  expect_equal(attr(in_1999_or_2000, "earliest_date"), dmy("01/01/1999"))
  expect_equal(attr(in_1999_or_2000, "latest_date"), dmy("31/12/2000"))

  my_result <- schedule(in_1999_or_2000)

  expected_result <- seq.Date(from = dmy("01/01/1999"),
                              to = dmy("31/12/2000"),
                              by = "1 day")

  expect_equal(my_result, expected_result)

})
