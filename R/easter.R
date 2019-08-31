#' Schedule dates during Easter
#'
#' @description
#' Creates a schedule of events occuring on the various Easter holidays
#' in the Gregorian calendar.
#'
#' `on_easter_weekend()` creates a schedule containing the union of the other
#' schedules.
#'
#' @details
#' The algorithm for the date of Easter is provided by the `timeDate` package.
#'
#' @keywords easter, schedule
#' @importFrom magrittr %>%
#' @return A schedule object.
#' @examples
#' schedule_days(on_easter_sunday(), from = 2010, to = 2020)
#'
#' schedule_days(on_good_friday(), from = 2010, to = 2020)
#'
#' schedule_days(on_easter_monday(), from = 2010, to = 2020)
#' @export

on_easter_sunday <- function(){on_easter_date(0)}

#' @export
#' @rdname on_easter_sunday

on_good_friday <- function(){on_easter_date(-2)}

#' @export
#' @rdname on_easter_sunday

on_easter_monday <- function(){on_easter_date(1)}

#' @export
#' @rdname on_easter_sunday

on_easter_saturday <- function(){on_easter_date(-1)}

#' @export
#' @rdname on_easter_sunday

on_easter_weekend <- function(){
  on_good_friday() %>%
    also_occur(on_easter_saturday()) %>%
    also_occur(on_easter_sunday()) %>%
    also_occur(on_easter_monday())
}

on_easter_date <- function(days_from_easter = 0){

  date_test <- function(date){

    year_of_date <- lubridate::year(date)
    easter_dates <- lubridate::as_date(timeDate::Easter(year_of_date))

    date == easter_dates + lubridate::days(days_from_easter)
  }

  out <- list(date_test = date_test)

  class(out) <- "schedule"

  out$n_terms <- 1

  out
}
