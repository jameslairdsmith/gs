make_element <- function(x, .f, ...){

  .fn <- function(date){
    .f(date, ...) == x
  }

  out <- list(name = "date_test",
              func = .fn)

  class(out) <- "date_element"

  out
}
