# function may need a better name.

anyday <- function(){

  out <- function(date){

    rep(TRUE, length(date))
  }

  class(out) <- "schedule"

  out
}
