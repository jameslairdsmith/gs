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
