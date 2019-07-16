#' Specify the weeks of a schedule
#'
#' Creates a schedule of events occuring in the weeks specified.
#'
#' @description `in_week()` returns the number of complete seven day periods
#' that have occurred between the date and January 1st, plus one.
#'
#' @param ... a numeric vector of week elements.
#'
#' @keywords month, date, scedule
#' @return A schedule of events occuring in the weeks specified.
#' @examples
#' my_dates <- c(as.Date("2000-01-01"),
#'               as.Date("2000-02-01"),
#'               as.Date("2000-03-01"),
#'               as.Date("2000-04-01"))
#'
#' my_dates
#'
#' in_january <- in_month("January")
#' in_january(my_dates)
#'
#' in_february <- in_month("Feb")
#' in_february(my_dates)
#'
#' in_march <- in_month(3)
#' in_march(my_dates)
#'
#' in_jan_or_feb <- in_month("Jan", "Feb")
#' in_jan_or_feb(my_dates)
#'
#' ## You can even mix your month specs.
#' in_jan_feb_mar <- in_month("January", "Feb", 3)
#' in_jan_feb_mar(my_dates)
#'
#' ## invalid inputs will produce an immediate error
#' \dontrun{
#' in_january <- in_month("Janu")
#' in_january <- in_month(13)}
#' @export

in_week <- function(x, ...){
  make_element(x, lubridate::week, ...)
}

#' @description The `in_isoweek()` function returns the week as it would appear in the ISO 8601
#'   system, which uses a reoccurring leap week.
#' @rdname in_week
#' @export

in_isoweek <- function(x, ...){
  make_element(x, lubridate::isoweek, ...)
}

in_epiweek <- function(x, ...){
  make_element(x, lubridate::epiweek, ...)
}
