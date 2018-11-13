on_quarter <- function(x, ...){
  occuring_on(x, lubridate::quarter, ...)
}

on_semester <- function(x, ...){
  occuring_on(x, lubridate::semester, ...)
}

on_year <- function(x, ...){
  occuring_on(x, lubridate::year, ...)
}

on_isoyear <- function(x, ...){
  occuring_on(x, lubridate::isoyear, ...)
}

on_epiyear <- function(x, ...){
  occuring_on(x, lubridate::epiyear, ...)
}

on_month <- function(x, label = TRUE, abbr = TRUE, ...){
  occuring_on(x, lubridate::month, label = label, abbr = abbr, ...)
}

on_wday <- function(x, label = TRUE, abbr = TRUE, ...){
  occuring_on(x, lubridate::wday, label = label, abbr = abbr, ...)
}

on_mday <- function(x, ...){
  occuring_on(x, lubridate::mday, ...)
}

on_yday <- function(x, ...){
  occuring_on(x, lubridate::yday, ...)
}

on_qday <- function(x, ...){
  occuring_on(x, lubridate::qday, ...)
}

on_week <- function(x, ...){
  occuring_on(x, lubridate::week, ...)
}

on_isoweek <- function(x, ...){
  occuring_on(x, lubridate::isoweek, ...)
}

on_epiweek <- function(x, ...){
  occuring_on(x, lubridate::epiweek, ...)
}

