#' Specify the month(s) of a schedule
#'
#' Creates a schedule of events occurring in the months specified.
#'
#' Months can be specified using their names (eg. "January"),
#' abbreviations (eg. "Jan") or integers (eg. 1).
#'
#' Multiple months can be specified in the same function call. For example:
#' `in_month(1, 3)` produces a schedule of events occurring in January and
#' March. The same thing is achieved by `in_month("Jan", "Mar")`.
#' Similarly `in_month(1:3)` produces a schedule of events occurring in
#' January, February and March.
#'
#' @param ... A character or numeric vector of month elements.
#'
#' @keywords month, date, schedule
#' @return A schedule object.
#' @examples
#' my_dates <- seq.Date(as.Date("2000-01-01"),
#'                      as.Date("2000-04-01"),
#'                      by = "1 month")
#'
#' is_occurring(my_dates, in_month("January"))
#'
#' is_occurring(my_dates, in_month("Feb"))
#'
#' is_occurring(my_dates, in_month(3))
#'
#' is_occurring(my_dates, in_month("Jan", "Mar"))
#'
#' is_occurring(my_dates, in_month(1:3))
#'
#' ## invalid inputs will produce an immediate error
#' \dontrun{
#' in_january <- in_month("Janu")
#' in_january <- in_month(0)}
#' @export


in_month <- function(...){

   x <- unlist(list(...))

   if(length(x) > 1) return(check_vec_loop(x, in_month))

   if(!(x %in% get_all_month_specs())){
      stop("x is not a legitimate month name")}

   if(x %in% 1:12){
      appro_function <- lubridate::month}

   if(x %in% get_month_names()){
      appro_function <- in_month_label_full}

   if(x %in% get_month_abbr_names()){
      appro_function <- in_month_label_abbr}

   make_element(x, appro_function)
}


in_month_label_abbr <- function(x){
   lubridate::month(x, label = TRUE, abbr = TRUE)
 }

in_month_label_full <- function(x){
   lubridate::month(x, label = TRUE, abbr = FALSE)
}

iden_func <- function(...){
   return(list(...))
}
