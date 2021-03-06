#' Specify the day(s) of a schedule
#'
#' @description
#' Creates a schedule of events occuring on the specified days of certain
#' periods.
#'
#' The type of period (week, month, quarter, year) is determined by the
#' function used (see *Details* section).
#'
#' @details
#' Each function creates a schedule where the events occur only on the specified
#' days within the given period:
#' * `on_mday` for days of the month. Eg:
#'     - `on_mday(1)` for the 1st day of each month.
#'     - `on_mday(15)` for the 15th day of each month.
#'     - `on_mday(31)` for the 31st day of each month (where that exists).
#' * `on_yday` for days of the year. Eg:
#'     - `on_yday(1)` for the 1st day of each year.
#'     - `on_yday(300)` for the 300th day of each year.
#'     - `on_yday(366)` for the 366th day of each year (where that exists).
#' * `on_qday` for days in the quarter. Eg:
#'     - `on_qday(1)` for the 1st day of every quarter.
#'     - `on_qday(45)` for the 45th day of every quarter.
#'     - `on_qday(92)` for the 92nd day of every quarter (where that exists).
#' * `on_wday` for days of the week.
#'     - These can be specified using numbers (where by default 1 is Sunday,
#'     see the *Arguments* section above.)
#'         - `on_wday(1)` for Sundays (by default).
#'         - `on_wday(7)` for Saturdays (by default).
#'     - Or using weekday names:
#'         - `on_wday("Sunday")` for Sundays.
#'         - `on_wday("Saturday")` for Saturdays.
#'         - `on_wday("Wednesday")` for Wednesdays.
#'     - Or their abbreviations:
#'         - `on_wday("Sun")` for Sundays.
#'         - `on_wday("Sat")` for Saturdays.
#'         - `on_wday("Wed")` for Wednesdays.
#'
#' All functions accept multiple day elements in a single call. For example
#' `on_mday(7, 9)` produces a schedule of dates occuring on the 7th and 9th
#' days of every month. Likewise `on_mday(1:5)` produces a schedule of dates
#' occuring on the first five days of every month. Similarly
#'  `on_wday("Tue", "Thu")` produces a schedule of events occuring every
#'  week on both Tuesdays and Thursdays.
#'
#' `on_weekend()` is a convenience function for
#' `on_wday("Sat", "Sun")`. `on_weekday()` is a convenience function for
#' `dont_occur(on_wday("Sat", "Sun"))`.
#'
#' @param ... A numeric vector of day specifications. In the case of `on_wday` the
#' elements can also be characters (see *Details* section below).
#' @param week_start If using the `on_wday` function with numeric day elements,
#' you can specify which ISO convention is used; 1 means Monday,
#' 7 means Sunday (default).
#'
#' @keywords month, week, day, date, schedule
#' @return A schedule object.
#' @examples
#'
#' my_dates <- seq.Date(from = as.Date("2000-01-01"),
#'                      to = as.Date("2000-12-01"),
#'                      by = "1 month")
#'
#' happen(on_mday(1), my_dates)
#'
#' happen(on_yday(1), my_dates)
#'
#' happen(on_qday(1), my_dates)
#'
#' happen(on_wday("Tue"), my_dates)
#'
#' happen(on_wday("Tue", "Thu"), my_dates)
#'
#' happen(on_weekend(), my_dates)
#' happen(on_weekday(), my_dates)
#'
#' ## Invalid inputs will produce an immediate error:
#' \dontrun{
#' on_mday(32)
#' on_wday(8)}
#' @export

on_mday <- function(...){

  x <- unlist(list(...))

  if(length(x) > 1) return(check_vec_loop(x, on_mday))

  if(!(x%%1==0)) stop("Month days can only be whole numbers")
  if(x > 31) stop("Month days cannot be greater than 31")
  if(x < 1) stop("Month days cannot be zero or negative")

  make_element(x, lubridate::mday)
}

#' @rdname on_mday
#' @export

on_yday <- function(...){

  x <- unlist(list(...))

  if(length(x) > 1) return(check_vec_loop(x, on_yday))

  if(!(x%%1==0)) stop("Year days can only be whole numbers")
  if(x > 366) stop("Year days cannot be greater than 366")
  if(x < 1) stop("Year days cannot be zero or negative")

  make_element(x, lubridate::yday)
}

#' @rdname on_mday
#' @export

on_qday <- function(...){

  x <- unlist(list(...))

  if(length(x) > 1) return(check_vec_loop(x, on_qday))

  if(!is_whole_number(x)) stop("Quarter days can only be whole numbers")
  if(x > 92) stop("Quarter days cannot be greater than 92")
  if(x < 1) stop("Quarter days cannot be zero or negative")

  make_element(x, lubridate::qday)
}

#' @rdname on_mday
#' @export

on_wday <- function(..., week_start = getOption("lubridate.week.start", 7)){

  x <- unlist(list(...))

  if(length(x) > 1) return(check_vec_loop(x, on_wday, week_start = week_start))

  if(!(x %in% get_all_day_specs())){
    stop("x is not a legitimate wday spec")}

  if(x %in% 1:7){
    partial_wday <- purrr::partial(lubridate::wday, week_start = week_start)
    appro_function <- partial_wday}

  if(x %in% get_day_names()){
    appro_function <- on_wday_label_full}

  if(x %in% get_day_abbr_names()){
    appro_function <- on_wday_label_abbr}

  make_element(x, appro_function)

}
#' @rdname on_mday
#' @export

on_weekend <- function(){

  out_func <- on_wday("Sat", "Sun")

  out_func
}

#' @rdname on_mday
#' @export

on_weekday <- function(){

  out_func <- dont_occur(on_wday("Sat", "Sun"))

  out_func
}

on_wday_label_abbr <- function(x, ...){
  lubridate::wday(x, label = T, abbr = T, ...)
}

on_wday_label_full <- function(x, ...){
  lubridate::wday(x, label = T, abbr = F, ...)
}


