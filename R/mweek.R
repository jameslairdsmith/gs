mweek <- function(x){
  first_day_of_month <- lubridate::make_date(year(x), month(x), 1L)
  (yday(x) - yday(first_day_of_month)) %/% 7 + 1
}

mweek <- function(x){
  first_day_of_month <- lubridate::make_date(year(x), month(x), 1L)
  (yday(x) - yday(first_day_of_month)) %/% 7 + 1
}

.other_week <- function(x, week_start) {
  x <- as.POSIXlt(x)
  date <- make_date(year(x), month(x), day(x))
  wday <- wday(x, week_start = week_start)
  date <- date + (4 - wday)
  jan1 <- as.numeric(make_date(year(date), 1, 1))
  1L + (as.numeric(date) - jan1) %/% 7L
}

# TODO

misoweek <- function(x) {
  .other_week(x, 1)
}

# TODO
mepiweek <- function(x) {
  .other_week(x, 7)
}

first_day_of_month <- function(x){
  lubridate::make_date(year(x), month(x), 1L)
}
