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
#' `doesnt_occur()` can accept either one or two schedules as input(s):
#' * When a single schedule is used, the function returns a schedule of all
#' events that do not fall on on the input schedule. The result is the
#' **negation** of the schedule. For example, the negation of all weekdays
#' would be a schedule of all weekend days.
#' * When two schedules are used, the function returns the first schedule but
#'  with the events of the second schedule stripped away. This works to remove
#'  the events of the second schedule from the first.
#'    - This means that `doesnt_occur(a, b)` is the equivalent of
#'      `only_occur(a, doesnt_occur(b))`
#'    - This usage works best when composing with the pipe (`%>%`) operator.
#'
#' @return A schedule object.
#' @examples
#' my_dates <- seq.Date(as.Date("1999-01-01"),
#'                      as.Date("1999-01-10"),
#'                      by = "1 day")
#'
#' happen(in_month("Jan"), my_dates)
#' happen(doesnt_occur(in_month("Jan")), my_dates)
#' happen(on_wday("Sat"), my_dates)
#' happen(doesnt_occur(on_wday("Sat")), my_dates)
#'
#' on_christmas <- only_occur(on_mday(25), in_month("Dec"))
#' on_new_years_day <- on_yday(1)
#' on_public_holidays <- also_occur(on_new_years_day, on_christmas)
#'
#' on_business_days <-
#'   doesnt_occur(on_weekend()) %>%
#'   doesnt_occur(on_public_holidays)
#'
#' happen(on_business_days, my_dates)
#' @export

doesnt_occur <- function(x, y = NULL){

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

    elem_1 %>% only_occur(out)
  }
}
