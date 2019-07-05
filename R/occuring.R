also_occuring <- function(elem_1, elem_2){

  out <- list(elem_2, elem_1)

  .fn <- function(date){
    elem_1(date) | elem_2(date)
  }

  out <- .fn

  class(out) <- c("schedule", "or_schedule")

  out

}

only_occuring <- function(elem_1, elem_2){

  out <- list(elem_2, elem_1)

  .fn <- function(date){
    elem_1(date) & elem_2(date)
  }

  out <- .fn

  class(out) <- c("schedule", "and_schedule")

  out
}

not_occuring <- function(elem_1, elem_2 = NULL){

  if(is.null(elem_2)){

    .fn <- function(date){
      !elem_1(date)
    }

    # out <- list(name = "not_schedule",
    #             func = .fn)

    out <- .fn

    class(out) <- c("not_schedule")

    out

  } else {

    .fn <- function(date){
      !elem_2(date)
    }

    out <- .fn

    class(out) <- c("not_schedule")


    elem_1 %>% only_occuring(out)
  }
}
