also_occuring <- function(elem_1, elem_2){

  out <- list(elem_2, elem_1)

  .fn <- function(date){
    elem_1$func(date) | elem_2$func(date)
  }

  out <- list(name = "schedule",
              func = .fn)

  class(out) <- c("schedule", "or_schedule")

  out

}

only_occuring <- function(elem_1, elem_2){

  out <- list(elem_2, elem_1)

  .fn <- function(date){
    elem_1$func(date) && elem_2$func(date)
  }

  out <- list(name = "schedule",
              func = .fn)

  class(out) <- c("schedule", "and_schedule")

  out
}

not_occuring <- function(elem_1, elem_2 = NULL){

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
