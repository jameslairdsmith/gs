#' Create a schedule of events occuring before or after other events
#'
#' @description
#' Creates a schedule of events occuring before or after a certain date or
#' another scheduled event within a certain period.
#'
#' @details
#' When `x` is a date.
#'
#' When `x` is a schedule...
#'
#' When `x` is a date, it saves having to...
#'
#' @param x Either a date object or a schedule.
#' @param within_given Required when
#'
#' @keywords after, before, date, schedule
#' @return A schedule of events occuring after or before the events specified.
#' @examples
#' x <- 1
#' x
#' @export

after <- function(x, within_given = NULL){

  if(is.Date(x)){

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
    applicable <- within_given(date_vec_changing) == within_given(date_vec_changing - days(1))
    out_vec <- vector(length = length(date))

    while(any(applicable)){

      date_vec_changing[applicable] <- date_vec_changing[applicable] - days(1)
      applicable <- within_given(date_vec_changing) == within_given(date_vec_changing - days(1))

      meet_criteria <- test_date(date_vec_changing, x)
      out_vec[meet_criteria] <- TRUE
    }
    out_vec
  }

  class(out) <- "schedule"

  out
}

#' @rdname after
#' @export

before <- function(x, within_given = NULL){

  if(is.Date(x)){

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
    applicable <- within_given(date_vec_changing) == within_given(date_vec_changing + days(1))
    out_vec <- vector(length = length(date))

    while(any(applicable)){

      date_vec_changing[applicable] <- date_vec_changing[applicable] + days(1)
      applicable <- within_given(date_vec_changing) == within_given(date_vec_changing + days(1))

      meet_criteria <- test_date(date_vec_changing, x)
      out_vec[meet_criteria] <- TRUE
    }
    out_vec
  }

  class(out) <- "schedule"

  out
}
