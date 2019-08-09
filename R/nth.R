#' Schedule the nth events within a period
#'
#' @description
#'
#' Get the nth event of a certain type within a given period.
#'
#' @param x,y Schedules to weave together.
#'
#' @details
#' R
#'
#' @return A schedule of events.
#' @examples
#' on_tuesday <- on_wday("Tue")
#'
#' on_fourth_tuesday <- on_nth(4, on_tuesday, within_given = "month")
#'
#' schedule(on_fourth_tuesday, during = 2000)
#'
#' @export

on_nth <- function(n, x, within_given, ...){

  if(is.character(within_given)){
    within_given <- strings_to_date_functions(within_given)
  }

  if(n > 0){
  out <- function(date){
    date_vec_changing <- date
    nth <- n
    applicable_within <- within_given(date_vec_changing) == within_given(date_vec_changing - lubridate::days(1))
    appicable_test <- test_date(date_vec_changing, x)
    applicable <- applicable_within & appicable_test
    num_vec <- vector(mode = "numeric", length = length(date_vec_changing))

    while(any(applicable)){

      date_vec_changing[applicable] <- date_vec_changing[applicable] - lubridate::days(1)


      meet_criteria <- test_date(date_vec_changing, x)
      num_vec[meet_criteria & applicable] <- num_vec[meet_criteria & applicable] + 1

      applicable_within <- within_given(date_vec_changing) == within_given(date_vec_changing - lubridate::days(1))
      applicable <- applicable_within & appicable_test
    }

    vec_nth <- num_vec + 1
    is_proper_value <- test_date(date, x)
    has_correct_prev_occurances <- vec_nth == nth

    meets_both_criteria <- is_proper_value & has_correct_prev_occurances

    meets_both_criteria

  }}

  if(n < 0){
    out <- function(date){
    date_vec_changing <- date
    nth <- abs(n)
    applicable_within <- within_given(date_vec_changing) == within_given(date_vec_changing + lubridate::days(1))
    appicable_test <- test_date(date_vec_changing, x)
    applicable <- applicable_within & appicable_test
    num_vec <- vector(mode = "numeric", length = length(date_vec_changing))

    while(any(applicable)){

      date_vec_changing[applicable] <- date_vec_changing[applicable] + lubridate::days(1)


      meet_criteria <- test_date(date_vec_changing, x)
      num_vec[meet_criteria & applicable] <- num_vec[meet_criteria & applicable] + 1

      applicable_within <- within_given(date_vec_changing) == within_given(date_vec_changing + lubridate::days(1))
      applicable <- applicable_within & appicable_test
    }

    vec_nth <- num_vec + 1
    is_proper_value <- test_date(date, x)
    has_correct_next_occurances <- vec_nth == nth

    meets_both_criteria <- is_proper_value & has_correct_next_occurances

    meets_both_criteria
    }}

  class(out) <- "schedule"

  out
}

#' @rdname on_nth
#' @export

on_first <- function(x, within_given, ...){
  on_nth(1, x, within_given, ...)
}

#' @rdname on_nth
#' @export

on_second <- function(x, within_given, ...){
  on_nth(2, x, within_given, ...)
}

#' @rdname on_nth
#' @export

on_third <- function(x, within_given, ...){
  on_nth(3, x, within_given, ...)
}

#' @rdname on_nth
#' @export

on_fourth <- function(x, within_given, ...){
  on_nth(4, x, within_given, ...)
}

#' @rdname on_nth
#' @export

on_last <- function(x, within_given, ...){
  on_nth(-1, x, within_given, ...)
}
