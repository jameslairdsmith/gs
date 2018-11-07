#' @rdname schedule_element
#' @export

on_element <- function(object, ...)
  UseMethod("on_element")


on_element.character <- function(character, .f, ...){

  out <- list(
    character = character,
    .f = .f,
    dots = ...
  )

  class(out) <- "on_element"

  out
}

on_element.default <- function(x, character, .f, ...){

  test_datetime <- any(is.Date(x) | is.POSIXct(x) | is.POSIXlt(x) | is.POSIXt(x))

  if(!test_datetime){
    stop("x must be a date or datetime", call. = F)
  }

  character == .f(x, ...)
}

on_element.schedule <- function(x, ...){

  #if(is.null(element) | !inherits(element, "on_element")){
    element <- on_element.character(...)
  #}

  x$elements[[length(x$elements) + 1]] <- element
  x
}
