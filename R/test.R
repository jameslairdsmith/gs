#' @rdname test_date
#' @export

test_date <- function(object, ...)
  UseMethod("test_date")

test_date.Date <- function(date, x, ...){

  test_date(x, date, ...)
}

test_date.date_element <- function(date_element, date, ...){

  list_of_results <-
    purrr::map(date_element$.f, purrr::exec, date) %>%
    purrr::modify_if(is.factor, as.character) %>%
    silent_equals_test(date_element$x) %>%
    ifelse(is.na(.), FALSE, .)

  any(list_of_results)

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
