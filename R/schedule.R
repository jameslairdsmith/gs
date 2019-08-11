#' Get the events of a schedule
#'
#' Get the events from a schedule object.
#'
#' @param ... A numeric vector of year elements.
#'
#' @keywords schedule
#' @return A date vector
#' @details R
#' @examples
#' on_first_day_month <- on_mday(1)
#'
#' schedule(on_first_day_month, during = 2000)
#' @export

schedule <- function(x, from = NULL, to = NULL, during = NULL, every = "day",...){

  from <- get_from(x = x, from = from, during = during)
  to <- get_to(x = x, to = to, during = during)

  #date_seq <- seq.Date(from = from, to = to, by = every)

  date_seq <- make_period_seq(start = from, end = to, period_unit = every)

  date_seq[test_date(date_seq, x)]
}

#' @rdname schedule
#' @export

schedule_days <- function(x, from = NULL, to = NULL, during = NULL, ...){

  schedule(x = x, from = from, to = to, during = during, every = "day", ...)
}

#' @rdname schedule
#' @export

schedule_hours <- function(x, from = NULL, to = NULL, during = NULL, ...){

  from <- get_from(x = x, from = from, during = during)
  to <- get_to(x = x, to = to, during = during)

  #to <- as_datetime(to)
  #from <- as_datetime(from)

  #datetime_seq <- seq.POSIXt(from = from, to = to, by = "1 hour")

  datetime_seq <- make_period_seq(start = from, end = to, period_unit = "hour")

  datetime_seq[test_date(datetime_seq, x)]
}

make_period_seq <- function(start, end, period_unit = "days"){

  one_period <- lubridate::period(num = 1, units = period_unit)

  my_interval <- interval(start - one_period , end + lubridate::days(1))
  num_periods <- (my_interval / one_period) - 2

  start + 0:num_periods * one_period
}
