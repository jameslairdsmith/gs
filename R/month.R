#' Create a month schedule
#'
#' Allows users to create an abstract object representing all dates taking
#' place within a
#' specific month. Users can specify which month using:
#' 1) The month name (eg. "January"),
#' 2) The month's abbreviation (eg. "Jan") or
#' 3) the month number within the year (eg. 1).
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
#' test_date(dmy("31/01/1990"), January)
#' @export


in_month <- function(...){

   char_vec <- unlist(list(...))

   if(length(char_vec) > 1){

      my_schedule <- in_month(char_vec[1])

      for(i in 2:length(char_vec)){
         my_schedule <- also_occuring(my_schedule, in_month(char_vec[i]))
      }
      return(my_schedule)
   }

   x <- unlist(list(...))

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
