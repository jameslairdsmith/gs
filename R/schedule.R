#' Get the events of a schedule
#'
#' Get the event dates or datetimes from a schedule.
#'
#' @param x A schedule object.
#' @param from,to,during The limits to place on the output (see *Details*).
#' @param period_type The period type of the output. Eg. "day" (the deafult),
#'  "hour" etc. Can be any unit accepted by `lubridate::period()`.
#' @param n The increment of the period type. Eg. for events occurring every
#'  half-hour the `period_type` should "minute" and `n` should be set to
#'  `30`.
#' @keywords schedule
#' @return A date or datetime object.
#' @details
#' `schedule_days()` is a convenience function for `schedule()` where
#' `period_type` is pre-filled as "day". Likewise for `schedule_hours()`,
#'  where `period_type` is pre-filled as "hour". These functions are
#'  recommended as they cover the most common use cases.
#'
#' The `from` and `to` arguments set limits on the output.
#' They are only required when the schedule `x` doesn't
#' have implicit limits (and therefore has an infinite number of possible
#' events).
#'
#' * `from` and `to` can each be either:
#'    + a single date or datetime value or,
#'    + A numeric year.
#'       - In the case of `from`, a numeric year translates to the
#'         start date of the year eg. `from = 2000` translates to
#'         `as.Date(2000-01-01)`.
#'       - In the case of `to` it translates to the end of the year eg.
#'         `to = 2001` translates to `as.Date(2001-12-31)`.
#' * `during` is a shortcut for when setting a single year limit. Eg.
#'   `during = 2000` is the equivalent of setting `from = as.Date(2000-01-01)`
#'   and `to = as.Date(2000-12-31)`.
#'
#' @examples
#' on_paydays <- on_mday(25)
#'
#' schedule_days(on_paydays,
#'               from = as.Date("2000-06-01"),
#'               to = as.Date("2000-12-01"))
#'
#' schedule_days(on_paydays, from = 2000, to = 2001)
#'
#' schedule_days(on_paydays, during = 2000)
#'
#' on_jan_paydays <- only_occurring(on_paydays, in_month("Jan"))
#'
#' schedule_hours(on_jan_paydays, during = 2000)
#'
#' on_jan_payday_2002 <-
#'    on_paydays %>%
#'    only_occurring(in_month("Jan")) %>%
#'    only_occurring(in_year(2002))
#'
#' ## No limits required
#'
#' schedule_days(on_jan_payday_2002)
#' schedule_hours(on_jan_payday_2002)
#'
#' schedule(on_jan_payday_2002, period_type = "minute", n = 30)
#' @export

schedule <- function(x,
                     from = NULL,
                     to = NULL,
                     during = NULL,
                     period_type = "day",
                     n = 1){

  from <- get_from(x = x, from = from, during = during)
  to <- get_to(x = x, to = to, during = during)

  date_seq <- make_period_seq(start = from,
                              end = to,
                              period_unit = period_type,
                              period_n = n)

  date_seq[test_date(date_seq, x)]
}

#' @rdname schedule
#' @export

schedule_days <- function(x, from = NULL, to = NULL, during = NULL){

  schedule(x = x, from = from, to = to, during = during, period_type = "day")
}

#' @rdname schedule
#' @export

schedule_hours <- function(x, from = NULL, to = NULL, during = NULL){

  schedule(x = x, from = from, to = to, during = during, period_type = "hour")
}



make_period_seq <- function(start, end, period_unit = "days", period_n = 1){

  one_period <- lubridate::period(num = period_n, units = period_unit)

  my_interval <- lubridate::interval(start - one_period , end + lubridate::days(1))
  num_periods <- (my_interval / one_period) - 2

  start + 0:num_periods * one_period
}
