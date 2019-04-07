on_nth <- function(n, x, within_given, ...){

  if(is.character(within_given)){
    within_given <- strings_to_date_functions(within_given)
  }

  output <- list(n = n,
                 x = x,
                 within_given = within_given)

  class(output) <- "date_range_element"

  output
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
