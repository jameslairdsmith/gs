#' @rdname test_date
#' @export

test_date <- function(object, ...)
  UseMethod("test_date")

test_date.Date <- function(date, x, ...){

  test_date(x, date, ...)
}

test_date.date_element <- function(date_element, date, ...){

  list_of_results <-
    purrr::map(date_element$.f, purrr::exec, date) %>%
    purrr::modify_if(is.factor, as.character) %>%
    silent_equals_test(date_element$x) %>%
    ifelse(is.na(.), FALSE, .)

  any(list_of_results)

}

test_date.date_range_element <- function(date_range_element, date, ...){

  initial_test_result <- test_date.date_element(date_range_element$x, date)

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
          if(test_date(date_element_to_be_tested, updated_date)){
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
      if(test_date(date_element_to_be_tested, updated_date)){
        n_instances <- n_instances - 1
      }
    }
    if(n == n_instances){return(TRUE)} else {return(FALSE)}
  }
}

test_date.schedule <- function(schedule, date, ...){

    out <-
      schedule %>%
      purrr::modify_if(is_date_element, test_date, date) %>%
      purrr::modify_if(is_schedule, test_date.schedule, date) %>%
      purrr::modify_if(is_schedule, recon)

    if(both_logical(out)){
      recon(out)
    } else {
      test_date.schedule(out, date)
    }
}
