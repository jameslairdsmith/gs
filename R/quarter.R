in_quarter <- function(x, ...){
  make_element(x, lubridate::quarter, ...)
}

in_semester <- function(x, ...){
  make_element(x, lubridate::semester, ...)
}
