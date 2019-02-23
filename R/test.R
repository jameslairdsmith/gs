#' @rdname test_date
#' @export

test_date <- function(object, ...)
  UseMethod("test_date")

test_date.Date <- function(date, x, ...){

  temp_function <- purrr::partial(date_eval, x, ...)
  #date_eval(x, date, ...)
  purrr::map_lgl(date, temp_function, ...)
}


date_eval <- function(object, ...)
  UseMethod("date_eval")

date_eval.date_element <- function(date_element, date, ...){

  list_of_results <-
    purrr::map(date_element$.f, purrr::exec, date) %>%
    purrr::modify_if(is.factor, as.character) %>%
    silent_equals_test(date_element$x) %>%
    ifelse(is.na(.), FALSE, .)

  any(list_of_results)

}

date_eval.date_range_element <- function(date_range_element, date, ...){

  initial_test_result <- date_eval.date_element(date_range_element$x, date)

  date_element_to_be_tested <- date_range_element$x
  n <- date_range_element$n
  date_to_be_tested_against <- date
  given_date_function <- date_range_element$within_given
  given_date_parameter <- given_date_function(date)

  if(initial_test_result == FALSE){
    return(FALSE)
  }

  if(n > 0){
      updated_date <- date - days(1)
      n_instances <- 1
      while(given_date_function(updated_date) == given_date_parameter){
        updated_date <- updated_date - days(1)
          if(date_eval(date_element_to_be_tested, updated_date)){
            n_instances <- n_instances + 1
          }
      }
    if(n == n_instances){return(TRUE)} else {return(FALSE)}
  }

  if(n < 0){
    updated_date <- date + days(1)
    n_instances <- -1
    while(given_date_function(updated_date) == given_date_parameter){
      updated_date <- updated_date + days(1)
      if(date_eval(date_element_to_be_tested, updated_date)){
        n_instances <- n_instances - 1
      }
    }
    if(n == n_instances){return(TRUE)} else {return(FALSE)}
  }
}

date_eval.schedule <- function(schedule, date, ...){

    out <-
      schedule %>%
      purrr::modify_if(is_date_element, date_eval, date) %>%
      purrr::modify_if(is_date_range_element, date_eval, date) %>%
      purrr::modify_if(is_schedule, date_eval.schedule, date) %>%
      purrr::modify_if(is_schedule, recon)

    if(both_logical(out)){
      recon(out)
    } else {
      date_eval.schedule(out, date)
    }
}
