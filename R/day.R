on_mday <- function(x, ...){
  make_element(x, lubridate::mday, ...)
}

on_yday <- function(x, ...){
  make_element(x, lubridate::yday, ...)
}

on_qday <- function(x, ...){
  make_element(x, lubridate::qday, ...)
}

on_wday <- function(x, label = TRUE, abbr = TRUE, ...){
  make_element(x, list(lubridate::wday,
                       on_wday_label_abbr,
                       on_wday_label_full), label = label, abbr = abbr, ...)
}

on_wday_label_abbr <- function(x){
  lubridate::wday(x, label = T, abbr = T)
}

on_wday_label_full <- function(x){
  lubridate::wday(x, label = T, abbr = F)
}
