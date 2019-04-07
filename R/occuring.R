occuring <- function(date_element){

  if(!is_temporal(date_element)){
    stop("both arguments must be either of type 'date_element' or 'schedule'", call. = F)
  }
  date_element
}

also_occuring <- function(elem_1, elem_2){

  out <- list(elem_2, elem_1)

  class(out) <- c("schedule", "or_schedule")

  out
}

only_occuring <- function(elem_1, elem_2){

  out <- list(elem_2, elem_1)

  class(out) <- c("schedule", "and_schedule")

  out
}

not_occuring <- function(elem_1, elem_2 = NULL){

  # out <- list(elem_1)
  #
  # class(out) <- c("not_schedule")
  #
  # out

  if(is.null(elem_2)){

    out <- list(elem_1)

    class(out) <- c("not_schedule")

    out

  } else {

    out <- list(elem_2)

    class(out) <- c("not_schedule")

    elem_1 %>% only_occuring(out)
  }
}
