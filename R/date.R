on_date <- function(x, ...){
  make_element(x, identity, ...)
}

after <- function(x, within_given, ...){

  output <- list(x = x,
                 within_given = within_given)

  class(output) <- "date_after_element"

  output
}

before <- function(x, within_given, ...){

  output <- list(x = x,
                 within_given = within_given)

  class(output) <- "date_before_element"

  output
}
