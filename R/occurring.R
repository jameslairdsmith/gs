#' Weave schedules together
#'
#' @description
#'
#' Functions to combine schedules using set operations.
#'
#' @details
#' * `also_occurring()` returns a schedule of events which includes all those
#' present in the first schedule (`x`) and all those present in the second
#' schedule (`y`). The resulting output is the **union** of the two schedules.
#' * `only_occurring()` returns a schedule of events which includes only those
#' present in both the first schedule (`x`) and in the second schedule
#' (`y`). The resulting output is the **intersection** of the two schedules.
#'
#' @param x,y Schedule objects.
#'
#' @details
#' Each function call is limited to two schedules. But more complex schedules
#' can be made by building schedules on top of one another. This process is
#' greatly eased by using the pipe operator (`%>%`) from the `magrittr`
#' package (see *Examples*).
#'
#' Behind the scenes, both function outputs inherit the schedule limits from
#' their inputs. For more
#' details see the [vignette](https://jameslairdsmith.github.io/scheduler/articles/understanding-schedule-limits.html)
#' on understanding schedule limits.
#'
#' @return A schedule object.
#' @examples
#' on_christmas <- only_occurring(on_mday(25), in_month("Dec"))
#'
#' schedule_days(on_christmas, from = 2000, to = 2004)
#'
#' on_new_years_day <- on_yday(1)
#'
#' on_boxing_day <-
#'    on_mday(26) %>%
#'    only_occurring(in_month("Dec"))
#'
#' on_public_holiday <-
#'    on_new_years_day %>%
#'    also_occurring(on_christmas) %>%
#'    also_occurring(on_boxing_day)
#'
#' schedule_days(on_public_holiday, from = 2000, to = 2004)
#' @export
also_occur <- function(x, y){

  elem_1 <- x
  elem_2 <- y

  #envoke_list <- list(elem_2, elem_1)

  out <- function(date){
    elem_1(date) | elem_2(date)
  }

  class(out) <- "schedule"

  out

}

#' @rdname also_occur
#' @export

only_occur <- function(x, y){

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

#' @export

also_occurring <- function(...){

  also_occur(...)
}

#' @export

only_occurring <- function(...){

  only_occur(...)
}
