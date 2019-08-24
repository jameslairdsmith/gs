#' Specify the date(s) of a schedule
#'
#' @description
#' Creates a schedule of events occurring on the dates specified.
#'
#' This function is best used when making ad-hoc changes to other schedules.
#'
#' @details
#' The function encodes limits in the output schedule. For more
#' details see the [vignette](https://jameslairdsmith.github.io/gs/articles/understanding-schedule-limits.html)
#' on understanding schedule limits.
#'
#' @param ... A vector of dates.
#'
#' @keywords date, schedule
#' @return A schedule object.
#' @examples
#' my_dates <- seq.Date(as.Date("2000-01-01"),
#'                      as.Date("2000-01-05"),
#'                      "1 day")
#'
#' on_first_day_millennium <- on_date(as.Date("2000-01-01"))
#' on_first_or_second_day_millennium <- on_date(as.Date("2000-01-01"),
#'                                              as.Date("2000-01-02"))
#'
#' happen(on_first_day_millennium, my_dates)
#' happen(on_first_or_second_day_millennium, my_dates)
#'
#' on_regular_paydays <- on_mday(25)
#' on_bonus_payday <- on_date(as.Date("2000-12-20"))
#'
#' on_paydays <- also_occur(on_bonus_payday, on_regular_paydays)
#'
#' schedule_days(on_paydays, during = 2000)
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
