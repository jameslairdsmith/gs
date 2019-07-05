make_element <- function(x, .f, ...){

  out <- function(date){
    .f(date, ...) == x
  }

  class(out) <- "schedule"

  out
}
