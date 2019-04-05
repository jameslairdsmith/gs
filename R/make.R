make_element <- function(x, .f, ...){

  # if(rlang::is_function(.f)){
  #   .f <- list(.f)
  # }

 # partial_func <- purrr::map(.f, purrr::partial, ..., .lazy = F)

  partial_func <- purrr::partial(.f, ..., .lazy = F)

  out <- list(x = x,
              .f = partial_func)

  class(out) <- "date_element"
  out
}
