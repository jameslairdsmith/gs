on_quarter <- function(x, ...){
  make_element(x, lubridate::quarter, ...)
}

on_semester <- function(x, ...){
  make_element(x, lubridate::semester, ...)
}
