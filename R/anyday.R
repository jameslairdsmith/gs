anyday <- function(){

  .fn <- function(date){

    rep(TRUE, length(date))
  }

  out <- list(name = "anyday_element",
              func = .fn)

  class(out) <- "anyday_element"

  out
}
