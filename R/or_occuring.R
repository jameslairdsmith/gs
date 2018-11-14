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

#or_occuring.on_element <- function(elem_1, elem_2){

  #out <- list(elem_1, elem_2)

  #class(out) <- "or_schedule"

  #out
#}

#or_occuring.or_schedule <- function(or_schedule, elem){

 # out <- list(
#    or_schedule,
#    elem
#  )

  #or_schedule[[length(or_schedule) + 1]] <- elem

#  class(out) <-  "or_schedule"

#  out

#}
