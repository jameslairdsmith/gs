make_element <- function(x, .f, ...){

  date_test <- function(date){
    .f(date, ...) == x
  }

  out <- list(date_test = date_test)

  class(out) <- "schedule"

  out$n_terms <- 1

  out
}
