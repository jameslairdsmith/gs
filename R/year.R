#' Specify the year(s) of a schedule
#'
#' Creates a schedule of events occuring in the year(s) specified.
#'
#' @param ... A numeric vector of year elements.
#'
#' @keywords year, date, schedule
#' @return A schedule of events occuring in the year(s) specified.
#' @details The `in_year()` function will retain information on the limits of
#' a schedule. If a schedule occurs only within a specified year or years, we
#' can infer the start date and end date of the schedule as being the first
#' day of a earliest year and the last day of the latest year respectively.
#' This means we do not need to specify these limits when passing the schedule
#' on to the `schedule()` function (see last of the examples below).
#' @examples
#' my_dates <- seq(from = as.Date("2000-01-01"),
#'                 to = as.Date("2004-01-01"),
#'                 by = "1 year")
#'
#' in_2000 <- in_year(2000)
#' in_2000(my_dates)
#'
#' in_2000_or_2001 <- in_year(2000, 2001)
#' in_2000_or_2001(my_dates)
#'
#' in_2000_or_2001_or_2002 <- in_year(2000:2002)
#' in_2000_or_2001_or_2002(my_dates)
#'
#' head(schedule(in_2000), 20)
#' @export

in_year <- function(...){

  x <- unlist(list(...))

  if(length(x) > 1){

    final_output <- check_vec_loop(x, in_year)

    earliest_date <- lubridate::make_date(year = min(x), month = 1, day = 1)
    latest_date <- lubridate::make_date(year = max(x), month = 12, day = 31)

    attr(final_output, "earliest_date") <- earliest_date
    attr(final_output, "latest_date") <- latest_date

    return(final_output)
    }

  output <- make_element(x, lubridate::year)

  earliest_date <- lubridate::make_date(year = min(x), month = 1, day = 1)
  latest_date <- lubridate::make_date(year = max(x), month = 12, day = 31)

  attr(output, "earliest_date") <- earliest_date
  attr(output, "latest_date") <- latest_date

  output
}

in_isoyear <- function(...){

  x <- unlist(list(...))

  if(length(x) > 1) return(check_vec_loop(x, in_isoyear))

  make_element(x, lubridate::isoyear)
}

in_epiyear <- function(...){

  x <- unlist(list(...))

  if(length(x) > 1) return(check_vec_loop(x, in_epiyear))

  make_element(x, lubridate::epiyear)
}
