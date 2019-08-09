#' Test whether a date falls on a schedule.
#'
#' Used to test.
#'
#' @param date A date object
#' @param x A schedule object
#' @keywords date, schedule
#'
#' @rdname test_date
#' @export

test_date <- function(date, schedule, ...){

  schedule(date)
}
