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
#' happen(on_saturday, my_dates)
#' happen(on_sunday, my_dates)
#' happen(on_weekend(), my_dates)
#' @export

happen <- function(schedule, date){

  schedule$date_test(date)
}
