or_occuring <- function(object, ...)
  UseMethod("or_occuring")

or_occuring.default <- function(elem_1, elem_2){

  if(!(inherits(elem_1, "date_element") | inherits(elem_1, "schedule"))){
    stop("both arguments must be either of type 'date_element' or 'schedule'", call. = F)
  }

  if(!(inherits(elem_2, "date_element") | inherits(elem_2, "schedule"))){
    stop("both arguments must be either of type 'date_element' or 'schedule'", call. = F)
  }

  out <- list(elem_1, elem_2)

  class(out) <- c("schedule","or_schedule")

  out

}
