#' Schedule dates during Easter
#'
#' @description
#' Creates a schedule of events occuring on the various Easter holidays.
#'
#' @details
#' The algorithm for the date of Easter is provided by the `timeDate` package.
#'
#' @keywords easter, schedule
#' @return A schedule object.
#' @examples
#' schedule_days(on_easter_sunday(), from = 2010, to = 2020)
#'
#' schedule_days(on_good_friday(), from = 2010, to = 2020)
#'
#' schedule_days(on_easter_saturday(), from = 2010, to = 2020)
#' @export

on_easter_sunday <- function(){

  date_test <- function(date){

    year_of_date <- lubridate::year(date)
    easter_dates <- lubridate::as_date(timeDate::Easter(year_of_date))

    date == easter_dates
  }

  out <- list(date_test = date_test)

  class(out) <- "schedule"

  out$n_terms <- 1

  out
}

#' @export
#' @rdname on_easter_sunday

on_good_friday <- function(){

  date_test <- function(date){

    year_of_date <- lubridate::year(date)
    easter_dates <- lubridate::as_date(timeDate::Easter(year_of_date))

    date == easter_dates - lubridate::days(2)
  }

  out <- list(date_test = date_test)

  class(out) <- "schedule"

  out$n_terms <- 1

  out
}

#' @export
#' @rdname on_easter_sunday

on_easter_monday <- function(){

  date_test <- function(date){

    year_of_date <- lubridate::year(date)
    easter_dates <- lubridate::as_date(timeDate::Easter(year_of_date))

    date == easter_dates + lubridate::days(1)
  }

  out <- list(date_test = date_test)

  class(out) <- "schedule"

  out$n_terms <- 1

  out
}
