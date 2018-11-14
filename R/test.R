#' @rdname test_date
#' @export

test_date <- function(object, ...)
  UseMethod("test_date")

test_date.date_element <- function(date_element, date, ...){

  if(!lubridate::is.instant(date)){
    stop("'date' argument must be of type 'Date' or 'POSIX'", call. = F)
  }

  date_element$x == date_element$.f(date)
}
