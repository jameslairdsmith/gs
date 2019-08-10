#' Invert a schedule
#'
#' @description
#'
#' These functions combine schedules by taking two of them as inputs and
#' returning a single combined schedule as output.
#'
#' @param x,y Schedules to weave together.
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
#'    - This means that `not_occurring(x, y)` is the equivalent of
#'      `only_occurring(x, not_occurring(y))`
#'
#' @return A schedule of events determined by the input schedules and rules
#' of the function used.
#' @examples
#' on_christmas <- only_occurring(on_mday(25), in_month("Dec"))
#' on_new_years_day <- on_yday(1)
#'
#' on_public_holidays <- also_occurring(on_new_years_day, on_christmas)
#'
#' schedule(on_public_holidays, from = 2000, to = 2004)
#'
#' weekday_public_holidays <- only_occurring(on_public_holidays,
#'                                          not_occurring(on_wday("Sat", "Sun")))
#'
#' schedule(weekday_public_holidays, from = 2000, to = 2004)
#'
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
