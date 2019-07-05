on_nth <- function(n, x, within_given, ...){

  if(is.character(within_given)){
    within_given <- strings_to_date_functions(within_given)
  }

  if(n > 0){
  .fn <- function(date){
    date_vec_changing <- date
    nth <- n
    applicable_within <- within_given(date_vec_changing) == within_given(date_vec_changing - days(1))
    appicable_test <- test_date(date_vec_changing, x)
    applicable <- applicable_within & appicable_test
    num_vec <- vector(mode = "numeric", length = length(date_vec_changing))

    while(any(applicable)){

      date_vec_changing[applicable] <- date_vec_changing[applicable] - days(1)


      meet_criteria <- test_date(date_vec_changing, x)
      num_vec[meet_criteria & applicable] <- num_vec[meet_criteria & applicable] + 1

      applicable_within <- within_given(date_vec_changing) == within_given(date_vec_changing - days(1))
      applicable <- applicable_within & appicable_test
    }

    vec_nth <- num_vec + 1
    is_proper_value <- test_date(date, x)
    has_correct_prev_occurances <- vec_nth == nth

    meets_both_criteria <- is_proper_value & has_correct_prev_occurances

    meets_both_criteria

  }}

  if(n < 0){
    .fn <- function(date){
    date_vec_changing <- date
    nth <- abs(n)
    applicable_within <- within_given(date_vec_changing) == within_given(date_vec_changing + days(1))
    appicable_test <- test_date(date_vec_changing, x)
    applicable <- applicable_within & appicable_test
    num_vec <- vector(mode = "numeric", length = length(date_vec_changing))

    while(any(applicable)){

      date_vec_changing[applicable] <- date_vec_changing[applicable] + days(1)


      meet_criteria <- test_date(date_vec_changing, x)
      num_vec[meet_criteria & applicable] <- num_vec[meet_criteria & applicable] + 1

      applicable_within <- within_given(date_vec_changing) == within_given(date_vec_changing + days(1))
      applicable <- applicable_within & appicable_test
    }

    vec_nth <- num_vec + 1
    is_proper_value <- test_date(date, x)
    has_correct_next_occurances <- vec_nth == nth

    meets_both_criteria <- is_proper_value & has_correct_next_occurances

    meets_both_criteria
  }}

  out <- .fn

  class(out) <- "nth_date_element"

  out
}

on_first <- function(x, within_given, ...){
  on_nth(1, x, within_given, ...)
}

on_second <- function(x, within_given, ...){
  on_nth(2, x, within_given, ...)
}

on_third <- function(x, within_given, ...){
  on_nth(3, x, within_given, ...)
}

on_fourth <- function(x, within_given, ...){
  on_nth(4, x, within_given, ...)
}

on_last <- function(x, within_given, ...){
  on_nth(-1, x, within_given, ...)
}
