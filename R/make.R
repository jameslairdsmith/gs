make_element <- function(x, .f, ...){

  partial_func <- purrr::partial(.f, ..., .lazy = F)

  out <- list(x = x,
              .f = partial_func)

  class(out) <- "date_element"
  out
}
