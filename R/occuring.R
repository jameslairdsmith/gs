occurs <- function(date_element){

  if(!is_temporal(date_element)){
    stop("both arguments must be either of type 'date_element' or 'schedule'", call. = F)
  }
  date_element
}

or_occurs <- function(elem_1, elem_2){

  if(!is_temporal(elem_1)){
    stop("both arguments must be either of type 'date_element' or 'schedule'", call. = F)
  }

  if(!is_temporal(elem_2)){
    stop("both arguments must be either of type 'date_element' or 'schedule'", call. = F)
  }

  out <- list(elem_1, elem_2)

  class(out) <- c("schedule", "or_schedule")

  out
}

and_occurs <- function(elem_1, elem_2){

  if(!is_temporal(elem_1)){
    stop("both arguments must be either of type 'date_element' or 'schedule'", call. = F)
  }

  if(!is_temporal(elem_2)){
    stop("both arguments must be either of type 'date_element' or 'schedule'", call. = F)
  }

  out <- list(elem_1, elem_2)

  class(out) <- c("schedule", "and_schedule")

  out
}
