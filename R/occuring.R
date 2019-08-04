#' Weave schedules together
#'
#' @description
#' Joins schedules together.
#'
#' @details
#' Details ...
#' @examples
#' on_christmas <- only_occuring(on_mday(25), in_month("Dec"))
#'
#' on_christmas(as.Date("2000-12-25"))
#' on_christmas(as.Date("2000-12-26"))
#'
#' schedule(on_christmas, from = 2000, to = 2001)
#'
#' on_new_years_day <- on_yday(1)
#'
#' on_public_holidays <- also_occuring(on_new_years_day, on_christmas)
#'
#' on_public_holidays(as.Date("2000-12-25"))
#' on_public_holidays(as.Date("2000-12-27"))
#' on_public_holidays(as.Date("2001-01-01"))
#'
#' schedule(on_public_holidays, from = 2000, to = 2001)
#'
#'
#' @export
also_occuring <- function(elem_1, elem_2){

  envoke_list <- list(elem_2, elem_1)

  out <- function(date){
    elem_1(date) | elem_2(date)
  }

  class(out) <- "schedule"

  out

}

#' @rdname also_occuring
#' @export

only_occuring <- function(elem_1, elem_2){

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

not_occuring <- function(elem_1, elem_2 = NULL){

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
