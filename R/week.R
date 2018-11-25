on_week <- function(x, ...){
  make_element(x, lubridate::week, ...)
}

on_isoweek <- function(x, ...){
  make_element(x, lubridate::isoweek, ...)
}

on_epiweek <- function(x, ...){
  make_element(x, lubridate::epiweek, ...)
}
