#' Specify the months of a schedule
#'
#' Creates a schedule of events occuring in the months specified.
#'
#' Months can be specified using their names (eg. "January"),
#' abbreviations (eg. "Jan") or integers (eg. 1).
#'
#' @param ... a character or numeric vector of month elements.
#'
#' @keywords month, date, scedule
#' @return A schedule of events occuring in the months specified.
#' @examples
#' my_dates <- c(as.Date("2000-01-01"),
#'               as.Date("2000-02-01"),
#'               as.Date("2000-03-01"),
#'               as.Date("2000-04-01"))
#'
#' my_dates
#'
#' in_january <- in_month("January")
#' in_january(my_dates)
#'
#' in_february <- in_month("Feb")
#' in_february(my_dates)
#'
#' in_march <- in_month(3)
#' in_march(my_dates)
#'
#' in_jan_or_feb <- in_month("Jan", "Feb")
#' in_jan_or_feb(my_dates)
#'
#' ## You can even mix your month specs, although I don't recommend it
#' in_jan_feb_mar <- in_month("January", "Feb", 3)
#' in_jan_feb_mar(my_dates)
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
