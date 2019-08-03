#' Specify the start and end of a schedule
#'
#' @description
#' Creates a schedule of events occuring before and/or after a certain date or
#' scheduled event.
#'
#' @details
#' When `x` is a date.
#'
#' When `x` is a year.
#'
#' When `x` is a schedule...
#'
#' When `x` is a date, it saves having to...
#'
#' `during()` is a convenience function for when events occur only within a
#' given year, inclusive of the first and last days.
#' Eg. for a schedule of events occuring only in the year 2000,
#' `during(2000)` can be used instead of
#' `in_between(as.Date("1999-12-31"), as.Date("2001-01-01"))`.
#'
#' @param start_event,end_event The start and/or end events of the schedule.
#' For each can be either be a date object or a schedule.
#' @param within_given A date accessor function. Required only when either
#' `start_date` or `end_date` are schedules. Puts a limit on how far into the
#'  future the schedule will extend. Used to avoid an infinite cycle.
#' @param year The year in which the events occcur.
#'
#' @keywords after, before, date, schedule
#' @return A schedule of events occuring before and/or after the events specified.
#' @examples
#' on_christmas <- on_mday(25) %>% only_occuring(in_month("Dec"))
#'
#' after_christmas <- after(on_christmas, within_given = lubridate::year)
#'
#' schedule(after_christmas, from = 2000, to = 2001)
#'
#' on_new_years_eve <- on_mday(31) %>% only_occuring(in_month("Dec"))
#'
#' in_between_christmas_and_new_years_eve <-
#'     in_between(on_christmas,
#'                on_new_years_eve,
#'                within_given = lubridate::year)
#'
#' schedule(in_between_christmas_and_new_years_eve, from = 2000, to = 2001)
#'
#' @export

after <- function(start_event, within_given = NULL){

  x <- start_event

  if(lubridate::is.Date(x)){

    out <- function(date){
      date > x
    }

    class(out) <- "schedule"

    attr(out, "earliest_date") <- x

    return(out)
  }

  if(is.character(within_given)){
    within_given <- strings_to_date_functions(within_given)
  }

  out <- function(date){

    date_vec_changing <- date
    applicable <- within_given(date_vec_changing) == within_given(date)
    out_vec <- vector(length = length(date))

    while(any(applicable)){

      date_vec_changing[applicable] <- date_vec_changing[applicable] - lubridate::days(1)
      applicable <- within_given(date_vec_changing) == within_given(date)

      meet_criteria <- test_date(date_vec_changing, x)
      out_vec[meet_criteria & applicable] <- TRUE
    }
    out_vec
  }

  class(out) <- "schedule"

  out
}

#' @rdname after
#' @export

before <- function(end_event, within_given = NULL){

  x <- end_event

  if(lubridate::is.Date(x)){

    out <- function(date){
      date < x
    }

    class(out) <- "schedule"

    attr(out, "latest_date") <- x

    return(out)
  }

  if(is.character(within_given)){
    within_given <- strings_to_date_functions(within_given)
  }

  out <- function(date){

    date_vec_changing <- date
    applicable <- within_given(date_vec_changing) == within_given(date)
    out_vec <- vector(length = length(date))

    while(any(applicable)){

      date_vec_changing[applicable] <- date_vec_changing[applicable] + lubridate::days(1)
      applicable <- within_given(date_vec_changing) == within_given(date)

      meet_criteria <- test_date(date_vec_changing, x)
      out_vec[meet_criteria & applicable] <- TRUE
    }
    out_vec
  }

  class(out) <- "schedule"

  out
}

#' @rdname after
#' @export

in_between <- function(start_event, end_event, within_given = NULL){

  if(is.numeric(start_event)){
    start_event <- lubridate::make_date(year = start_event)
  }

  if(is.numeric(end_event)){
    end_event <- lubridate::make_date(year = end_event, month = 12, day = 31)
  }

  only_occuring(after(start_event, within_given = within_given),
                before(end_event, within_given = within_given))
}

#' @rdname after
#' @export

during <- function(year){

  start_event <- lubridate::make_date(year = year)
  end_event <- lubridate::make_date(year = year, month = 12, day = 31)

  only_occuring(after(start_event),
                before(end_event))

}
