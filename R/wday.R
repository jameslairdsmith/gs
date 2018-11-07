#' @rdname on_wday
#' @export

on_wday <- function(object, ...)
  UseMethod("on_wday")


on_wday.character <- function(character, ...){

  out <- list(
    character = character,
    .f = ~lubridate::wday(),
    dots = ...
  )

  class(out) <- "on_wday"

  out
}

on_wday.default <- function(x, character, ...){

  test_datetime <- any(is.Date(x) | is.POSIXct(x) | is.POSIXlt(x) | is.POSIXt(x))

  if(!test_datetime){
    stop("x must be a date or datetime", call. = F)
  }

  character == lubridate::wday(x, ...)
}

