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

schedule <- function(x, from = NULL, to = NULL, during = NULL, every = "1 day",...){

  from <- get_from(x = x, from = from, during = during)
  to <- get_to(x = x, to = to, during = during)

  date_seq <- seq.Date(from = from, to = to, by = every)

  date_seq[test_date(date_seq, x)]
}

#' @rdname schedule
#' @export

schedule_days <- function(x, from = NULL, to = NULL, during = NULL, ...){

  schedule(x = x, from = from, to = to, during = during, every = "1 day", ...)
}

#' @rdname schedule
#' @export

schedule_hours <- function(x, from = NULL, to = NULL, during = NULL, ...){

  from <- get_from(x = x, from = from, during = during)
  to <- get_to(x = x, to = to, during = during)

  to <- as_datetime(to)
  from <- as_datetime(from)

  datetime_seq <- seq.POSIXt(from = from, to = to, by = "1 hour")

  datetime_seq[test_date(datetime_seq, x)]
}
