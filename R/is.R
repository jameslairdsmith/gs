#' Does a date fall on a schedule?
#'
#' @description
#' Tests whether a date falls on a schedule.
#' @param date a date-time object
#' @param schedule A schedule object
#' @keywords date, schedule
#' @return A logical.
#' @examples
#' on_saturdays <- on_wday("Sat")
#'
#' is_occurring(as.Date("2000-01-01"), on_saturdays)
#' is_occurring(as.Date("2000-01-02"), on_saturdays)
#' @export

is_occurring <- function(date, schedule){

  schedule(date)
}
