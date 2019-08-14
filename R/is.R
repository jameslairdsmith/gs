#' Does a date fall on a schedule?
#'
#' @description
#' Tests whether a date falls on a schedule.
#' @param date a date-time object
#' @param schedule A schedule object
#' @keywords date, schedule
#' @return A logical.
#' @examples
#' my_dates <- seq.Date(from = as.Date("2000-01-01"),
#'                      to = as.Date("2000-01-10"),
#'                      by = "1 day")
#'
#' on_saturdays <- on_wday("Sat")
#'
#' is_occurring(my_dates, on_saturdays)
#' @export

is_occurring <- function(date, schedule){

  schedule(date)
}
