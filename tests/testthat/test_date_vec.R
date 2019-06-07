context("test_date_vec")

# test_that("test_date() works on a vector", {
#
#   date_vector <- c(dmy("01/01/1990"), dmy("02/01/1990"), dmy("03/01/1990"))
#   first_day_of_year <- on_yday(1)
#
#   expect_equal(test_date(date_vector, first_day_of_year), c(TRUE, FALSE, FALSE))
# })
#
# test_that("test_date() on and_schedule works on a vector", {
#
#   date_vector <- c(dmy("01/01/2000"), dmy("02/01/2000"), dmy("03/01/2000"))
#   first_day_of_mil <- on_yday(1) %>% only_occuring(in_year(2000))
#
#   expect_equal(test_date(date_vector, first_day_of_mil), c(TRUE, FALSE, FALSE))
# })
#
# test_that("test_date() on or_schedule works on a vector", {
#
#   date_vector <- c(dmy("01/01/2000"), dmy("02/01/2000"), dmy("03/01/2000"))
#   first_day_of_mil <- on_yday(1) %>% also_occuring(in_year(2000))
#
#   expect_equal(test_date(date_vector, first_day_of_mil), c(TRUE, TRUE, TRUE))
# })
#
# test_that("test_date() on nested schedule works on a vector", {
#
#   date_vector <- c(dmy("01/01/2000"), dmy("02/01/2000"), dmy("03/01/2000"))
#   first_day_of_mil_jan <-
#     on_yday(1) %>%
#     only_occuring(in_year(2000)) %>%
#     only_occuring(in_month("Jan"))
#
#   expect_equal(test_date(date_vector, first_day_of_mil_jan), c(TRUE, FALSE, FALSE))
# })
