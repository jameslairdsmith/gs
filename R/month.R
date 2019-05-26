#' Create a month element
#'
#' Allows users to create object that embody all dates taking place within a
#' specific month. Users can specify which month using either 1) The month
#' name (eg. "January"), the month's abbreviation (eg. "Jan") or the month
#' number within the year (eg. 1).
#'
#'
#' @param x The name, abbreviation or number of the month.
#' @param override_name_check Whether to allow month names and abbreviations
#' not found in in your default local (not recommended).
#' @keywords month, date, scedule
#' @importFrom magrittr %>%
#' @importFrom lubridate month

#' @return A month date element
#' @examples
#' library(magrittr)
#' library(lubridate)
#'
#'January <- in_month("Jan")
#'
#' test_date.Date(dmy("31/01/1990"), January)
#' @export


in_month <- function(x, override_name_check = FALSE, ...){


  # if(override_name_check == FALSE){



   if(!(x %in% get_all_month_specs())){
      stop("x is not a legitimate month name")}
   #}

   if(x %in% 1:12){
      appro_function <- lubridate::month}

   if(x %in% get_month_names()){
      appro_function <- in_month_label_full}

   if(x %in% get_month_abbr_names()){
      appro_function <- in_month_label_abbr}

   # make_element(x, list(lubridate::month,
   #                      in_month_label_abbr,
   #                      in_month_label_full), ...)

   make_element(x, appro_function, ...)
}


 in_month_label_abbr <- function(x){
   lubridate::month(x, label = TRUE, abbr = TRUE)
 }

 in_month_label_full <- function(x){
   lubridate::month(x, label = TRUE, abbr = FALSE)
 }
