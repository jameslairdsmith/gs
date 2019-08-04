#' Specify the quarter(s) of a schedule
#'
#' @description
#' Creates a schedule of events occuring in the quarters or semester specified.
#'
#' @param ...  A numeric vector of quarter specifications.
#' @param x Numeric specifying which semester the events occur in (1 or 2).
#' @param fiscal_start Numeric indicating the starting month of a fiscal year.
#' Defaults to 1 (January).
#'
#' @keywords semester, quarter, date, schedule
#' @return A schedule of events occuring in the quarters or semester specified.
#' @examples
#'  my_dates <- seq.Date(from = as.Date("2000-01-01"),
#'                      to = as.Date("2000-12-01"),
#'                      by = "1 month")
#'
#' in_q1 <- in_quarter(1)
#' in_fiscal_q1 <- in_quarter(1, fiscal_start = 2)
#'
#' in_q1(my_dates)
#' in_fiscal_q1(my_dates)
#'
#' in_q1_or_q2 <- in_quarter(1, 2)
#' in_fiscal_q1_or_q2 <- in_quarter(1, 2, fiscal_start = 2)
#'
#' in_q1_or_q2(my_dates)
#' in_fiscal_q1_or_q2(my_dates)
#'
#' in_second_semester <- in_semester(2)
#'
#' in_second_semester(my_dates)
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
