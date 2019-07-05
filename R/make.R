make_element <- function(x, .f, ...){

  .fn <- function(date){
    .f(date, ...) == x
  }

  out <- .fn

  class(out) <- "date_element"

  out
}
