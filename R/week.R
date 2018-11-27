in_week <- function(x, ...){
  make_element(x, lubridate::week, ...)
}

in_isoweek <- function(x, ...){
  make_element(x, lubridate::isoweek, ...)
}

in_epiweek <- function(x, ...){
  make_element(x, lubridate::epiweek, ...)
}
