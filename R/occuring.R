#' Weave schedules together
#'
#' @description
#'
#' These functions combine schedules by taking two of them as inputs and
#' returning a single combined schedule as output.
#'
#' @param x,y Schedules to weave together.
#'
#' @details
#' `also_occuring()` returns a schedule of events which includes all those
#' present in the first schedule (`x`) and all those present in the second
#' schedule (`y`). The results is the **union** of the two schedules.
#'
#' `only_occuring()` returns a schedule of events which includes only those
#' present in both the first schedule (`x`) and in the second schedule
#' (`y`). The result is the **intersection** of the two schedules.
#'
#' `not_occuring()` can accept either one or who schedules as input(s):
#' * When a single schedule is used, the function returns a schedule of all
#' events that do not fall on on the input schedule. The result is the
#' **negation** of the schedule. For example, the negation of all weekdays
#' would be a schedule of all weekend days.
#' * When two schedules are used, the function returns the first schedule but
#'  with the events of the second schedule stripped away. This works to remove
#'  the events of the second schedule from the first.
#'    - This means that `not_occuring(x, y)` is the equivalent of
#'      `only_occuring(x, not_occuring(y))`
#'
#' @return A schedule of events determined by the input schedules and rules
#' of the function used.
#' @examples
#' on_christmas <- only_occuring(on_mday(25), in_month("Dec"))
#'
#' schedule(on_christmas, from = 2000, to = 2004)
#'
#' on_new_years_day <- on_yday(1)
#'
#' on_public_holidays <- also_occuring(on_new_years_day, on_christmas)
#'
#' schedule(on_public_holidays, from = 2000, to = 2004)
#'
#' weekday_public_holidays <- only_occuring(on_public_holidays,
#'                                          not_occuring(on_wday("Sat", "Sun")))
#'
#' schedule(weekday_public_holidays, from = 2000, to = 2004)
#'
#' @export
also_occuring <- function(x, y){

  elem_1 <- x
  elem_2 <- y

  #envoke_list <- list(elem_2, elem_1)

  out <- function(date){
    elem_1(date) | elem_2(date)
  }

  class(out) <- "schedule"

  out

}

#' @rdname also_occuring
#' @export

only_occuring <- function(x, y){

  elem_1 <- x
  elem_2 <- y

  #envoke_list <- list(elem_2, elem_1)

  out <- function(date){
    elem_1(date) & elem_2(date)
  }

  class(out) <- "schedule"

  if("latest_date" %in% get_attribute_names(elem_1)){
    attr(out, "latest_date") <- attr(elem_1, "latest_date")
  }
  if("earliest_date" %in% get_attribute_names(elem_1)){
    attr(out, "earliest_date") <- attr(elem_1, "earliest_date")
  }
  if("latest_date" %in% get_attribute_names(elem_2)){
    attr(out, "latest_date") <- attr(elem_2, "latest_date")
  }
  if("earliest_date" %in% get_attribute_names(elem_2)){
    attr(out, "earliest_date") <- attr(elem_2, "earliest_date")
  }
  if("latest_date" %in% get_attribute_names(elem_1) &
     "latest_date" %in% get_attribute_names(elem_2)){
    attr(out, "latest_date") <- max(attr(elem_1, "latest_date"),
                                    attr(elem_2, "latest_date"))
  }
  if("earliest_date" %in% get_attribute_names(elem_1) &
     "earliest_date" %in% get_attribute_names(elem_2)){
    attr(out, "earliest_date") <- min(attr(elem_1, "earliest_date"),
                                      attr(elem_2, "earliest_date"))
  }


  out
}

#' @rdname also_occuring
#' @export

not_occuring <- function(x, y = NULL){

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

    elem_1 %>% only_occuring(out)
  }
}
