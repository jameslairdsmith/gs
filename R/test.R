#' @rdname test_date
#' @export

test_date <- function(object, ...)
  UseMethod("test_date")

test_date.Date <- function(x, date_element, ...){
  date_element$x == date_element$.f(x)
}

test_date.POSIXct <- function(x, character, .f, ...){
  test_date.Date(x, character, .f, ...)
}

test_date.POSIXlt <- function(x, character, .f, ...){
  test_date.Date(x, character, .f, ...)
}

test_date.POSIXt <- function(x, character, .f, ...){
  test_date.Date(x, character, .f, ...)
}
