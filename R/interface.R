is_wday <- function(x, date, ...){
  x == lubridate::wday(date, ...)
}
