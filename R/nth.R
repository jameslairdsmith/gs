#' Schedule the nth events of a period
#'
#' @description
#'
#' Given a schedule of events, create a new schedule of only the nth occurrences
#'  within a given period.
#'
#' @param n A single integer specifying which event to select. A negative
#' integer will select the nth last event  (i.e. `-1L` will select the
#' last event of the period, `-2L` will return the second last etc).
#' @param x A schedule from which to select events.
#' @param within_given A period from within which to select events. Can be
#' either:
#'  * A string shortcut for a period type: either `"day"`, `"week"`, `"month"`,
#'    `"quarter"` or `"year"`. Or,
#'  * A date accessor function. Eg. `lubridate::month()`.
#'
#' @details
#' Convenience functions are provided for the first, second, third, fourth and
#' last events of a given period.
#'
#' All functions accept arbitrarily complex schedules. For example, if you
#' wanted a schedule of events occurring on the 12th either Tuesday or Thursday
#' of the quarter you would use:
#' `on_nth(12, on_wday("Tue", "Thu"), within_given = "quarter")`.
#'
#' @return A schedule object.
#' @examples
#' tuesday <- on_wday("Tue")
#'
#' second_tuesday_month <- on_second(tuesday, within_given = "month")
#' schedule_days(second_tuesday_month, during = 2000)
#'
#' last_tuesday_year <- on_last(tuesday, within_given = "year")
#' schedule_days(last_tuesday_year, during = 2000)
#'
#' tenth_tuesday_quarter <- on_nth(10, tuesday, within_given = "quarter")
#' schedule_days(tenth_tuesday_quarter, during = 2000)
#'
#' tues_or_thurs <- on_wday("Tue", "Thu")
#'
#' tenth_tues_or_thurs_quarter <- on_nth(10, tues_or_thurs, within_given = "quarter")
#' schedule_days(tenth_tues_or_thurs_quarter, during = 2000)
#'
#' @export

on_nth <- function(n, x, within_given){

  if(is.character(within_given)){
    within_given <- strings_to_date_functions(within_given)
  }

  if(n > 0){
  date_test <- function(date){
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
    date_test <- function(date){
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

  out <- list(date_test = date_test)

  class(out) <- "schedule"

  out$n_terms <- 1

  out
}

#' @rdname on_nth
#' @export

on_first <- function(x, within_given){
  on_nth(1, x, within_given)
}

#' @rdname on_nth
#' @export

on_second <- function(x, within_given){
  on_nth(2, x, within_given)
}

#' @rdname on_nth
#' @export

on_third <- function(x, within_given){
  on_nth(3, x, within_given)
}

#' @rdname on_nth
#' @export

on_fourth <- function(x, within_given){
  on_nth(4, x, within_given)
}

#' @rdname on_nth
#' @export

on_last <- function(x, within_given){
  on_nth(-1, x, within_given)
}
