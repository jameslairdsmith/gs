on_date <- function(x, ...){

  out <- make_element(x, identity, ...)

  class(out) <- "date_element"

  out
}

after <- function(x, within_given, ...){

  if(is.character(within_given)){
    within_given <- strings_to_date_functions(within_given)
  }

  .fn <- function(date){

    date_element_to_be_tested <- x
    date_to_be_tested_against <- date
    given_date_function <- within_given
    given_date_parameter <- given_date_function(date)

    updated_date <- date - days(1)
    n_occurances <- 0

    while(given_date_function(updated_date) == given_date_parameter){

      if(test_date(updated_date, date_element_to_be_tested)){
         return(TRUE)
       }
        updated_date <- updated_date - days(1)
       }
       FALSE
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

  output <- list(x = x,
                 within_given = within_given)

  class(output) <- "date_before_element"

  output
}
