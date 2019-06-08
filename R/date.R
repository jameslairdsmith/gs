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

  # .fn <- function(date){
  #
  #   date_element_to_be_tested <- x
  #   date_to_be_tested_against <- date
  #   given_date_function <- within_given
  #   given_date_parameter <- given_date_function(date_to_be_tested_against)
  #
  #   updated_date <- date_to_be_tested_against
  #
  #   logical_out <- vector("logical", length = length(date_to_be_tested_against))
  #
  #   while(any(given_date_function(updated_date) == given_date_parameter)){
  #
  #     i <- which(given_date_function(updated_date) == given_date_parameter)
  #
  #     updated_date[i] <- updated_date[i] - days(1)
  #
  #     which_to_change <- which(test_date(updated_date[i], date_element_to_be_tested))
  #
  #     logical_out[which_to_change] <- TRUE
  #   }
  #   logical_out
  # }

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
