#' @rdname test_date
#' @export

test_date <- function(object, ...)
  UseMethod("test_date")

test_date.date_element <- function(date_element, date, ...){

  date_element$x == date_element$.f(date)
}

test_date.schedule <- function(schedule, date, ...){

    out <-
      schedule %>%
      purrr::modify_if(is_date_element, test_date, date) %>%
      purrr::modify_if(is_schedule, test_date.schedule, date) %>%
      purrr::modify_if(is_schedule, recon)

    if(both_logical(out)){
      recon(out)
    } else {
      test_date.schedule(out, date)
    }
}
