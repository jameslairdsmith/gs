on_quarter <- function(x, ...){
  make_element(x, lubridate::quarter, ...)
}

on_semester <- function(x, ...){
  make_element(x, lubridate::semester, ...)
}

on_year <- function(x, ...){
  make_element(x, lubridate::year, ...)
}

on_isoyear <- function(x, ...){
  make_element(x, lubridate::isoyear, ...)
}

on_epiyear <- function(x, ...){
  make_element(x, lubridate::epiyear, ...)
}

on_month <- function(x, label = TRUE, abbr = TRUE, ...){
  make_element(x, lubridate::month, label = label, abbr = abbr, ...)
}

on_wday <- function(x, label = TRUE, abbr = TRUE, ...){
  make_element(x, lubridate::wday, label = label, abbr = abbr, ...)
}

on_mday <- function(x, ...){
  make_element(x, lubridate::mday, ...)
}

on_yday <- function(x, ...){
  make_element(x, lubridate::yday, ...)
}

on_qday <- function(x, ...){
  make_element(x, lubridate::qday, ...)
}

on_week <- function(x, ...){
  make_element(x, lubridate::week, ...)
}

on_isoweek <- function(x, ...){
  make_element(x, lubridate::isoweek, ...)
}

on_epiweek <- function(x, ...){
  make_element(x, lubridate::epiweek, ...)
}

