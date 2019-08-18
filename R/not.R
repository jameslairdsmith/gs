#' Invert a schedule
#'
#' @description
#'
#' Create a schedule of events occurring only when the specified events do
#' not occur.
#'
#' @param x,y Schedule objects.
#'
#' @details
#' `not_occuring()` can accept either one or two schedules as input(s):
#' * When a single schedule is used, the function returns a schedule of all
#' events that do not fall on on the input schedule. The result is the
#' **negation** of the schedule. For example, the negation of all weekdays
#' would be a schedule of all weekend days.
#' * When two schedules are used, the function returns the first schedule but
#'  with the events of the second schedule stripped away. This works to remove
#'  the events of the second schedule from the first.
#'    - This means that `not_occurring(a, b)` is the equivalent of
#'      `only_occurring(a, not_occurring(b))`
#'    - This usage works best when composing with the pipe (`%>%`) operator.
#'
#' @return A schedule object.
#' @examples
#' my_dates <- seq.Date(as.Date("1999-01-01"),
#'                      as.Date("1999-01-10"),
#'                      by = "1 day")
#'
#' is_occurring(my_dates, in_month("Jan"))
#' is_occurring(my_dates, not_occurring(in_month("Jan")))
#' is_occurring(my_dates, on_wday("Sat"))
#' is_occurring(my_dates, not_occurring(on_wday("Sat")))
#'
#' on_christmas <- only_occurring(on_mday(25), in_month("Dec"))
#' on_new_years_day <- on_yday(1)
#' on_public_holidays <- also_occurring(on_new_years_day, on_christmas)
#'
#' on_business_days <-
#'   not_occurring(on_weekend()) %>%
#'   not_occurring(on_public_holidays)
#'
#' is_occurring(my_dates, on_business_days)
#' @export

not_occurring <- function(x, y = NULL){

  elem_1 <- x
  elem_2 <- y

  if(is.null(elem_2)){

    out <- function(date){
      !elem_1(date)
    }

    class(out) <- c("schedule")

    out

  } else {

    out <- function(date){
      !elem_2(date)
    }

    class(out) <- c("schedule")

    elem_1 %>% only_occurring(out)
  }
}
