on_nth <- function(n, x, within, ...){

  output <- list(n = n,
                 x = x,
                 within = within)

  class(output) <- "date_interval_element"

  output
}

on_first <- function(x, within, ...){
  on_nth(1, x, within, ...)
}

on_second <- function(x, within, ...){
  on_nth(2, x, within, ...)
}

on_last <- function(x, within, ...){
  on_nth(-1, x, within, ...)
}
