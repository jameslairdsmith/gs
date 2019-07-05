anyday <- function(){

  .fn <- function(date){

    rep(TRUE, length(date))
  }

  out <- .fn

  class(out) <- "anyday_element"

  out
}
