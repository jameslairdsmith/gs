on_date <- function(...){

  x <- unlist(list(...))

  if(length(x) > 1){

    my_schedule <- check_vec_loop(x, on_date)

    return(my_schedule)
  }

  out <- make_element(x, identity)

  class(out) <- "date_element"

  out
}

after <- function(x, within_given = NULL){

  if(is.Date(x)){

    .fn <- function(date){
      date > x
    }

    out <- .fn

    class(out) <- "date_after_element"

    return(out)
  }

  if(is.character(within_given)){
    within_given <- strings_to_date_functions(within_given)
  }

  .fn <- function(date){

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

  out <- .fn

  class(out) <- "date_after_element"

  out
}

before <- function(x, within_given = NULL){

  if(is.Date(x)){

    .fn <- function(date){
      date < x
    }

    out <- .fn

    class(out) <- "date_before_element"

    return(out)
  }

  if(is.character(within_given)){
    within_given <- strings_to_date_functions(within_given)
  }

  .fn <- function(date){

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

  out <- .fn

  class(out) <- "date_after_element"

  out
}
