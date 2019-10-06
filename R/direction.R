#' Specify the start and end of a schedule
#'
#' @description
#' Creates a schedule of events occuring before or after either a certain
#' date or another scheduled event.
#'
#' @details
#' When `start_event` and/or `end_event` are schedules they are likely to come
#' around every year.  To have any events which can occur 'before' or 'after'
#' them, they need a limit specified by the `within_given` argument. This can
#'  either be date accessor function like `lubridate::month()` or
#'  `lubridate::year()` or the provided character shortcuts
#'  `"day"`, `"week"`, `"month"`, `"quarter"` or `"year"`.
#'
#' @param start_event,end_event The start and/or end events of the schedule.
#' Can be either:
#' * A date-time object or
#' * A schedule.
#'
#' @param within_given Required only when either `start_date` or `end_date`
#' is a schedule. Can be either:
#'  * A string shortcut for a period type: either `"day"`, `"week"`, `"month"`,
#'    `"quarter"` or `"year"`. Or,
#'  * A date accessor function. Eg. `lubridate::month()`.
#'
#' @keywords after, before, date, schedule
#' @return A schedule object.
#' @examples
#' christmas <-  only_occur(in_month("Dec"), on_mday(25))
#' new_years_eve <-  only_occur(in_month("Dec"), on_mday(31))
#'
#' after_christmas <- after(christmas, within_given = "year")
#'
#' schedule_days(after_christmas, from = 2000, to = 2001)
#'
#' in_between_christmas_and_new_years_eve <-
#'     in_between(christmas,
#'                new_years_eve,
#'                within_given = lubridate::year)
#'
#' schedule_days(in_between_christmas_and_new_years_eve, from = 2000, to = 2001)
#'
#' @export

after <- function(start_event, within_given = NULL){

  x <- start_event

  if(lubridate::is.instant(x)){

    date_test <- function(date){
      date > x
    }

    out <- list(date_test = date_test)

    class(out) <- "schedule"

    attr(out, "earliest_date") <- x +lubridate::days(1)

    return(out)
  }

  if(is.character(within_given)){
    within_given <- strings_to_date_functions(within_given)
  }

  date_test <- function(date){

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

  out <- list(date_test = date_test)

  class(out) <- "schedule"

  out$n_terms <- 1

  out
}

#' @rdname after
#' @export

before <- function(end_event, within_given = NULL){

  x <- end_event

  if(lubridate::is.instant(x)){

    date_test <- function(date){
      date < x
    }

    out <- list(date_test = date_test)

    class(out) <- "schedule"

    attr(out, "latest_date") <- x - lubridate::days(1)

    return(out)
  }

  if(is.character(within_given)){
    within_given <- strings_to_date_functions(within_given)
  }

  date_test <- function(date){

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

  out <- list(date_test = date_test)

  class(out) <- "schedule"

  out$n_terms <- 1

  out
}

#' @rdname after
#' @export

in_between <- function(start_event, end_event, within_given = NULL){

  only_occur(after(start_event, within_given = within_given),
             before(end_event, within_given = within_given))
}

#' @rdname after
#' @export

on_or_after <- function(start_event, within_given = NULL){

  x <- start_event

  if(lubridate::is.instant(x)){

    date_test <- function(date){
      date >= x
    }

    out <- list(date_test = date_test)

    class(out) <- "schedule"

    attr(out, "earliest_date") <- x

    return(out)
  }

  also_occur(start_event, after(start_event, within_given = within_given))
}

#' @rdname after
#' @export

on_or_before <- function(end_event, within_given = NULL){

  x <- end_event

  if(lubridate::is.instant(x)){

    date_test <- function(date){
      date <= x
    }

    out <- list(date_test = date_test)

    class(out) <- "schedule"

    attr(out, "latest_date") <- x

    return(out)
  }

  also_occur(end_event, before(end_event, within_given = within_given))
}
