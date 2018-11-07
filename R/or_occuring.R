or_occuring <- function(object, ...)
  UseMethod("or_occuring")

or_occuring.on_element <- function(elem_1, elem_2){

  out <- list(elem_1, elem_2)

  class(out) <- "or_schedule"

  out
}

or_occuring.or_schedule <- function(or_schedule, elem){

  out <- list(
    or_schedule,
    elem
  )

  #or_schedule[[length(or_schedule) + 1]] <- elem

  class(out) <-  "or_schedule"

  out

}
