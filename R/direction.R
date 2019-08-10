#' Specify the start and end of a schedule
#'
#' @description
#' Creates a schedule of events occuring before and/or after a certain date or
#' scheduled event.
#'
#' @details
#' When `x` is a date.
#'
#' When `x` is a schedule...
#'
#' When `x` is a date, it saves having to...
#'
#' @param start_event,end_event The start and/or end events of the schedule.
#' For each can be either be a date object or a schedule.
#' @param within_given A date accessor function. Required only when either
#' `start_date` or `end_date` are schedules. Puts a limit on how far into the
#'  future the schedule will extend. Used to avoid an infinite cycle.
#'
#' @keywords after, before, date, schedule
#' @return A schedule of events occuring before and/or after the events specified.
#' @examples
#' on_christmas <- on_mday(25) %>% only_occurring(in_month("Dec"))
#' on_new_years_eve <- on_mday(31) %>% only_occurring(in_month("Dec"))
#'
#' after_christmas <- after(on_christmas, within_given = lubridate::year)
#'
#' schedule(after_christmas, from = 2000, to = 2001)
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

    attr(out, "earliest_date") <- x +lubridate::days(1)

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

    attr(out, "latest_date") <- x - lubridate::days(1)

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

  only_occurring(after(start_event, within_given = within_given),
                before(end_event, within_given = within_given))
}

#' @rdname after
#' @export

on_or_after <- function(start_event, within_given = NULL){

  x <- start_event

  if(lubridate::is.Date(x)){

    out <- function(date){
      date >= x
    }

    class(out) <- "schedule"

    attr(out, "earliest_date") <- x

    return(out)
  }

  also_occurring(start_event, after(start_event, within_given = within_given))
}

#' @rdname after
#' @export

on_or_before <- function(end_event, within_given = NULL){

  x <- end_event

  if(lubridate::is.Date(x)){

    out <- function(date){
      date <= x
    }

    class(out) <- "schedule"

    attr(out, "latest_date") <- x

    return(out)
  }

  also_occurring(end_event, before(end_event, within_given = within_given))
}
