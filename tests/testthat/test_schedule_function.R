context("test_schedule_function")

test_that("schedule function works", {

  christmas <- on_mday(25) %>% only_occuring(in_month("Dec"))

  one <- christmas %>% schedule(from = dmy("01/01/1990"), to = dmy("31/12/1992"))

  two <- seq.Date(from = dmy("25/12/1990"), to = dmy("25/12/1992"), by = "1 year")

  expect_identical(one, two)
})


test_that("schedule function works with year abbreviations", {

  christmas <- on_mday(25) %>% only_occuring(in_month("Dec"))

  one <- christmas %>% schedule(from = 1990, to = 1992)

  two <- seq.Date(from = dmy("25/12/1990"), to = dmy("25/12/1992"), by = "1 year")

  expect_identical(one, two)
})

test_that("schedule function works when specifying only before() and after()", {

  demo_dates <- seq.Date(from = dmy("02/07/1990"), to = dmy("30/07/1990"), by = "1 day")

  before_and_after <- after(dmy("01/07/1990")) %>% only_occuring(before(dmy("31/07/1990")))

  expect_true("earliest_date" %in% get_attribute_names(before_and_after))
  expect_true("latest_date" %in% get_attribute_names(before_and_after))

  expect_equal(schedule(before_and_after), demo_dates)

})
