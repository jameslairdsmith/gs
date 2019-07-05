also_occuring <- function(elem_1, elem_2){

  envoke_list <- list(elem_2, elem_1)

  out <- function(date){
    elem_1(date) | elem_2(date)
  }

  class(out) <- "schedule"

  out

}

only_occuring <- function(elem_1, elem_2){

  envoke_list <- list(elem_2, elem_1)

  out <- function(date){
    elem_1(date) & elem_2(date)
  }

  class(out) <- "schedule"

  out
}

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
