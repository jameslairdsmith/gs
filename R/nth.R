on_nth <- function(n, x, within, ...){

  output <- list(n = n,
                 x = x,
                 within = within)

  class(output) <- "date_range_element"

  output
}

on_first <- function(x, within, ...){
  on_nth(1, x, within, ...)
}

on_second <- function(x, within, ...){
  on_nth(2, x, within, ...)
}

on_third <- function(x, within, ...){
  on_nth(3, x, within, ...)
}

on_fourth <- function(x, within, ...){
  on_nth(4, x, within, ...)
}

on_last <- function(x, within, ...){
  on_nth(-1, x, within, ...)
}
