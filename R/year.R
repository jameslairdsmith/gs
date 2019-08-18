#' Specify the year(s) of a schedule
#'
#' Creates a schedule of events occurring in the year(s) specified.
#'
#' @param ... A numeric vector of years.
#'
#' @keywords year, date, schedule
#' @return A schedule object.
#' @details
#' The type of year is determined by the function used. These year types are
#' built atop their [definitions][lubridate::year()] in the `lubridate`
#' package, where `year()` is the commonly understood definition of
#' a year. The other definitions from the `lubridate` package are quoted here:
#'
#'  * `isoyear()` returns years according to the ISO 8601 week calendar.
#'  * `epiyear()` returns years according to the epidemilogical week calendars.
#'
#' The `in_year()` function will encode limits on the output schedule.
#' For more details see the
#' [vignette](https://jameslairdsmith.github.io/scheduler/articles/understanding-schedule-limits.html)
#' on understanding schedule limits.
#' @examples
#' my_dates <- seq(from = as.Date("2000-01-01"),
#'                 to = as.Date("2005-01-01"),
#'                 by = "1 year")
#'
#' is_occurring(my_dates, in_year(2000))
#'
#' is_occurring(my_dates, in_year(2000, 2002))
#'
#' is_occurring(my_dates, in_year(2000:2002))
#'
#' is_occurring(my_dates, in_year(2000:2002, 2005))
#'
#' is_occurring(my_dates, in_isoyear(2004))
#'
#' head(schedule(in_year(2000), 20))
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

#' @rdname in_year
#' @export

in_isoyear <- function(...){

  x <- unlist(list(...))

  if(length(x) > 1) return(check_vec_loop(x, in_isoyear))

  make_element(x, lubridate::isoyear)
}

#' @rdname in_year
#' @export

in_epiyear <- function(...){

  x <- unlist(list(...))

  if(length(x) > 1) return(check_vec_loop(x, in_epiyear))

  make_element(x, lubridate::epiyear)
}
