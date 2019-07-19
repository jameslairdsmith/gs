#' Specify the days of a schedule
#'
#' @description
#' Creates a schedule of events occuring on the types of days specified.
#'
#' @details
#' * `on_mday` creates a schedule where the events occur only on the specified
#' days of the month. Eg. `on_mday(1)` for schedules with events occuring on
#' the first day of every month. `on_mday(31)` for schedules with events
#' occuring on the 31st day of every month (where that exists).
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
#'         - `on_wday(1)` for Sundays.
#'         - `on_wday(7)` for Saturdays.
#'     - Or using weekday names:
#'         - `on_wday("Sunday")` for Sundays.
#'         - `on_wday("Saturday")` for Saturdays.
#'         - `on_wday("Wednesday")` for Wednesdays.
#'     - Or their abbreviations:
#'         - `on_wday("Sun")` for Sundays.
#'         - `on_wday("Sat")` for Saturdays.
#'         - `on_wday("Wed")` for Wednesdays.
#'
#' Users can specify multiple day elements in a single call. For example
#' `on_mday(1, 3)` produces a schedule of dates occuring on the 1st and 3rd
#' days of every month. Likewise `on_wday("Tue", "Thu")` produces a schedule
#' of events occuring every week on both Tuesdays and Thursdays.
#'
#' `on_weekend()` is a convenience function for
#' `on_wday("Sat", "Sun")`
#'
#' @param ... A numeric vector of day elements. In the case of `on_wday` the
#' elements can also be characters (see details below).
#' @param week_start If using the `on_wday` function with numeric day elements,
#' you can specify which ISO convention is used; 1 means Monday,
#' 7 means Sunday (default)
#'
#' You can use `lubridate`'s global option
#' `lubridate.week.start` to set this parameter globally.
#'
#' @keywords month, date, scedule
#' @return A schedule of events occuring on the day types specified.
#' @export

on_mday <- function(...){

  x <- unlist(list(...))

  if(length(x) > 1) return(check_vec_loop(x, on_mday))

  make_element(x, lubridate::mday)
}

#' @rdname on_mday
#' @export

on_yday <- function(...){

  x <- unlist(list(...))

  if(length(x) > 1) return(check_vec_loop(x, on_yday))

  make_element(x, lubridate::yday)
}

#' @rdname on_mday
#' @export

on_qday <- function(...){

  x <- unlist(list(...))

  if(length(x) > 1) return(check_vec_loop(x, on_qday))

  make_element(x, lubridate::qday)
}

#' @rdname on_mday
#' @export

on_wday <- function(..., week_start = getOption("lubridate.week.start", 7)){

  x <- unlist(list(...))

  if(length(x) > 1) return(check_vec_loop(x, on_wday, week_start = week_start))

  if(!(x %in% get_all_day_specs())){
    stop("x is not a legitimate wday name")}

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
  on_wday("Sat", "Sun")
}

on_wday_label_abbr <- function(x, ...){
  lubridate::wday(x, label = T, abbr = T, ...)
}

on_wday_label_full <- function(x, ...){
  lubridate::wday(x, label = T, abbr = F, ...)
}


