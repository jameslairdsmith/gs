library(lubridate)

test_that("on_easter_sunday() function works", {
  expect_true(happen(on_easter_sunday(), dmy("12/04/2020")))
  expect_false(happen(on_easter_sunday(), dmy("13/04/2020")))
  expect_false(happen(on_easter_sunday(), dmy("11/04/2020")))

  expect_true(happen(on_easter_sunday(), dmy("21/04/2019")))
  expect_false(happen(on_easter_sunday(), dmy("20/04/2019")))
  expect_false(happen(on_easter_sunday(), dmy("22/04/2019")))

  expect_true(happen(on_easter_sunday(), dmy("15/04/2001")))
  expect_false(happen(on_easter_sunday(), dmy("16/04/2001")))
  expect_false(happen(on_easter_sunday(), dmy("14/04/2001")))

  expect_true(happen(on_easter_sunday(), dmy("23/03/2008")))
  expect_false(happen(on_easter_sunday(), dmy("24/03/2008")))
  expect_false(happen(on_easter_sunday(), dmy("22/04/2008")))

  expect_true(happen(on_easter_sunday(), dmy("15/04/1900")))
  expect_false(happen(on_easter_sunday(), dmy("16/04/1900")))
  expect_false(happen(on_easter_sunday(), dmy("14/04/1900")))

  expect_true(happen(on_easter_sunday(), dmy("21/04/1935")))
  expect_false(happen(on_easter_sunday(), dmy("22/04/1935")))
  expect_false(happen(on_easter_sunday(), dmy("20/04/1935")))
})
