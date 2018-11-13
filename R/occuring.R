#' @rdname occuring_on
#' @export

occuring_on <- function(object, ...)
  UseMethod("occuring_on")

occuring_on.default <- function(x, .f, ...){

  partial_func <- purrr::partial(.f, ..., .lazy = F)

  out <- list(x = x,
              .f = partial_func)

  class(out) <- "date_element"
  out
}
