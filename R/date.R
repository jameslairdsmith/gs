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

after <- function(x, within_given, ...){

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

  out <- list(name = "date_after_element",
              func = .fn)

  class(out) <- "date_after_element"

  out
}

before <- function(x, within_given, ...){

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

  out <- list(name = "date_after_element",
              func = .fn)

  class(out) <- "date_after_element"

  out
}

after_date <- function(hard_date){

  .fn <- function(date){
    date > hard_date
  }

  out <- list(name = "after_date_element",
              func = .fn)

  class(out) <- "after_date_element"

  out
}

before_date <- function(hard_date){

  .fn <- function(date){
    date < hard_date
  }

  out <- list(name = "before_date_element",
              func = .fn)

  class(out) <- "before_date_element"

  out
}
