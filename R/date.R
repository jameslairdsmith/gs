on_date <- function(...){

  x <- unlist(list(...))

  if(length(x) > 1) return(check_vec_loop(x, on_date))

  out <- make_element(x, identity)

  class(out) <- "schedule"

  out
}

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
