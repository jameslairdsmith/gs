in_year <- function(x, ...){
  make_element(x, lubridate::year, ...)
}

in_isoyear <- function(x, ...){
  make_element(x, lubridate::isoyear, ...)
}

in_epiyear <- function(x, ...){
  make_element(x, lubridate::epiyear, ...)
}
