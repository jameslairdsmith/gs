#' Specify the quarter(s) of a schedule
#'
#' @description
#' Creates a schedule of events occurring in the quarters or semester specified.
#'
#' @param ...  A numeric vector of quarter specifications.
#' @param x Numeric specifying which semester the events occur in (1 or 2).
#' @param fiscal_start For fiscal quarters, a numeric indicating the starting
#' month of a fiscal year. Defaults to 1 (January).
#'
#' @keywords semester, quarter, date, schedule
#' @return A schedule object.
#' @examples
#'  my_dates <- seq.Date(from = as.Date("2000-01-01"),
#'                      to = as.Date("2000-12-01"),
#'                      by = "1 month")
#'
#' is_occurring(my_dates, in_quarter(1))
#'
#' is_occurring(my_dates, in_quarter(1, 3))
#'
#' is_occurring(my_dates, in_quarter(1:3))
#'
#' is_occurring(my_dates, in_quarter(1, 3, fiscal_start = 2))
#'
#' is_occurring(my_dates, in_semester(2))
#' @export

in_quarter <- function(..., fiscal_start = 1){

  x <- unlist(list(...))

  if(length(x) > 1) return(check_vec_loop(x,
                                          in_quarter,
                                          fiscal_start = fiscal_start))

  make_element(x, lubridate::quarter, fiscal_start = fiscal_start)

}

#' @rdname in_quarter
#' @export

in_semester <- function(x){

  make_element(x, lubridate::semester)

}
