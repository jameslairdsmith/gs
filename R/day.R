#' Specify the days of a schedule
#'
#' Creates a schedule of events occuring on the types of days specified.
#'
#' `on_weekend()` is a convenience function for
#' `on_wday("Sat", "Sun")`
#'
#' @param ... a numeric vector of day elements. In the case of `on_wday` the
#' elements can also be characters (see details below).
#' @param week_start
#' You can use `lubridate`'s global option `lubridate.week.start` to set this
#' parameter globally
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


