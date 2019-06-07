on_every <- function(n, x, starting, ending){


  output <- list(n = n,
                 x = x,
                 starting = starting,
                 ending = ending)

  class(output) <- "date_every_element"

  output
}
