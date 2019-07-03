anyday <- function(){

  .fn <- function(date){
    date <- TRUE
  }

  out <- list(name = "anyday_element",
              func = .fn)

  class(out) <- "anyday_element"

  out
}
