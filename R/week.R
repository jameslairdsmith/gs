#' Specify the week(s) of a schedule
#'
#' @description
#'
#' Creates a schedule of events occurring in the weeks specified.
#'
#' @details
#'
#' The type of week is determined by the function used. These week types are
#' built atop their definitions from the `lubridate` package, which are quoted
#' here:
#' * `week()` returns the number of complete seven day periods that have
#' occurred between the date and January 1st, plus one.
#'
#' @param ... a numeric vector of week specifications.
#'
#' @keywords week, date, scedule
#' @return A schedule of events occurring in the weeks specified.
#' @examples
#' my_dates <- seq.Date(from = as.Date("2000-01-01"),
#'                      to = as.Date("2000-02-01"),
#'                      by = "1 week")
#' my_dates
#'
#' in_first_week_year <- in_week(1)
#' in_first_week_year(my_dates)
#'
#' in_first_or_third_week_year <- in_week(1, 3)
#' in_first_or_third_week_year(my_dates)
#'
#' in_first_three_weeks_year <- in_week(1:3)
#' in_first_three_weeks_year(my_dates)
#'
#' in_first_isoweek_year <- in_isoweek(1)
#' in_first_isoweek_year(my_dates)
#'
#' ## invalid inputs will produce an immediate error:
#' \dontrun{
#' in_week(0)
#' in_week(54)
#' in_week(1.5)}
#' @export

in_week <- function(...){

  x <- unlist(list(...))

  if(length(x) > 1) return(check_vec_loop(x, in_week))

  check_week_num(x)

  make_element(x, lubridate::week)

}

#' @details * `isoweek()` returns the week as it would appear in the ISO 8601
#' system, which uses a reoccurring leap week.
#' @rdname in_week
#' @export

in_isoweek <- function(...){

  x <- unlist(list(...))

  if(length(x) > 1) return(check_vec_loop(x, in_isoweek))

  check_week_num(x)

  make_element(x, lubridate::isoweek)
}

#' @details * `epiweek()` is the US CDC version of epidemiological week. It
#' follows same rules as `isoweek()` but starts on Sunday. In other parts of
#' the world the convention is to start epidemiological weeks on Monday,
#' which is the same as isoweek.
#' @rdname in_week
#' @export

in_epiweek <- function(...){

  x <- unlist(list(...))

  if(length(x) > 1) return(check_vec_loop(x, in_epiweek))

  check_week_num(x)

  make_element(x, lubridate::epiweek)
}

check_week_num <- function(week_num){
  if(week_num < 1){stop("Week number cannot be less than 1", call. = F)}
  if(week_num > 53){stop("Week number cannot be greater than 53", call. = F)}
  if((week_num%%1) != 0){stop("Week number must be an integer", call. = F)}
}
