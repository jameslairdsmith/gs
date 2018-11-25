on_year <- function(x, ...){
  make_element(x, lubridate::year, ...)
}

on_isoyear <- function(x, ...){
  make_element(x, lubridate::isoyear, ...)
}

on_epiyear <- function(x, ...){
  make_element(x, lubridate::epiyear, ...)
}
