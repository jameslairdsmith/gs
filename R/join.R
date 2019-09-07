#' Join schedules together
#'
#' @description
#'
#' Functions to combine schedules using set operations.
#'
#' @details
#' * `also_occur()` returns a schedule of events which includes all those
#' present in the first schedule (`x`) and all those present in the second
#' schedule (`y`). The resulting output is the **union** of the two schedules.
#' * `only_occur()` returns a schedule of events which includes only those
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
#' @return A schedule object.
#' @examples
#' library(magrittr, warn.conflicts = FALSE)
#' on_christmas <- only_occur(on_mday(25), in_month("Dec"))
#'
#' schedule_days(on_christmas, from = 2000, to = 2004)
#'
#' on_new_years_day <- on_yday(1)
#'
#' on_boxing_day <-
#'    on_mday(26) %>%
#'    only_occur(in_month("Dec"))
#'
#' on_public_holiday <-
#'    on_new_years_day %>%
#'    also_occur(on_christmas) %>%
#'    also_occur(on_boxing_day)
#'
#' schedule_days(on_public_holiday, from = 2000, to = 2004)
#' @export
also_occur <- function(x, y){

  elem_1 <- x$date_test
  elem_2 <- y$date_test

  #envoke_list <- list(elem_2, elem_1)

  date_test <- function(date){
    elem_1(date) | elem_2(date)
  }

  out <- list(date_test = date_test)

  class(out) <- "schedule"

  out$n_terms <- x$n_terms + y$n_terms

  out

}

#' @rdname also_occur
#' @export

only_occur <- function(x, y){

  elem_1 <- x$date_test
  elem_2 <- y$date_test

  #envoke_list <- list(elem_2, elem_1)

  date_test <- function(date){
    elem_1(date) & elem_2(date)
  }

  out <- list(date_test = date_test)

  class(out) <- "schedule"

  out$n_terms <- x$n_terms + y$n_terms

  if("latest_date" %in% get_attribute_names(x)){
    attr(out, "latest_date") <- attr(x, "latest_date")
  }
  if("earliest_date" %in% get_attribute_names(x)){
    attr(out, "earliest_date") <- attr(x, "earliest_date")
  }
  if("latest_date" %in% get_attribute_names(y)){
    attr(out, "latest_date") <- attr(y, "latest_date")
  }
  if("earliest_date" %in% get_attribute_names(y)){
    attr(out, "earliest_date") <- attr(y, "earliest_date")
  }
  if("latest_date" %in% get_attribute_names(x) &
     "latest_date" %in% get_attribute_names(y)){
    attr(out, "latest_date") <- max(attr(x, "latest_date"),
                                    attr(y, "latest_date"))
  }
  if("earliest_date" %in% get_attribute_names(x) &
     "earliest_date" %in% get_attribute_names(y)){
    attr(out, "earliest_date") <- min(attr(x, "earliest_date"),
                                      attr(y, "earliest_date"))
  }


  out
}
