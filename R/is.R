#' Does a date fall on a schedule?
#'
#' @description
#' Does a date fall on a schedule?
#' @param date a date-time object
#' @param schedule A schedule object
#' @keywords date, schedule
#' @return TRUE or FALSE depending on whether the date falls on the schedule.
#' @examples
#' my_dates <- seq.Date(from = as.Date("2000-01-01"),
#'                      to = as.Date("2000-01-10"),
#'                      by = "1 day")
#'
#' on_saturday <- on_wday("Sat")
#' on_sunday <- on_wday("Sun")
#'
#' is_occurring(my_dates, on_saturday)
#' is_occurring(my_dates, on_sunday)
#' is_occurring(my_dates, on_weekend())
#' @export

is_occurring <- function(date, schedule){

  schedule(date)
}
