#' Test whether a date falls on a schedule.
#'
#' Deprecated. Instead use `is_occurring()`.
#'
#' @param date A date object
#' @param schedule A schedule object

test_date <- function(date, schedule, ...){

  schedule(date)
}
