#' Specify the date(s) of a schedule
#'
#' @description
#' Creates a schedule of events occuring on the dates specified.
#'
#' @details
#' This function is best used when making changes to other schedules where
#' those changes fall on defined dates.
#'
#' @param ... A vector of dates.
#'
#' @keywords date, schedule
#' @return A schedule of events occuring on the dates specified.
#' @examples
#'
#' on_first_day_millennium <- on_date(as.Date("2000-01-01"))
#' on_first_or_second_day_millennium <- on_date(as.Date("2000-01-01"),
#'                                              as.Date("2000-01-02"))
#'
#' on_first_day_millennium(as.Date("2000-01-01"))
#' on_first_day_millennium(as.Date("2000-01-02"))
#' on_first_or_second_day_millennium(as.Date("2000-01-01"))
#' on_first_or_second_day_millennium(as.Date("2000-01-02"))
#' on_first_or_second_day_millennium(as.Date("2000-01-03"))
#' @export

on_date <- function(...){

  x <- unlist(list(...))

  if(length(x) > 1) return(check_vec_loop(x, on_date))

  out <- make_element(x, identity)

  class(out) <- "schedule"

  attr(out, "earliest_date") <- lubridate::as_date(x)

  attr(out, "latest_date") <- lubridate::as_date(x)

  out
}
