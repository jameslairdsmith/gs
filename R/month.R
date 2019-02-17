in_month <- function(x, override_name_check = FALSE, ...){

   if(override_name_check == FALSE){

   if(!(x %in% get_all_month_specs())){
      stop("x is not a legitimate month name")
   }}

   make_element(x, list(lubridate::month,
                        in_month_label_abbr,
                        in_month_label_full), ...)
}


 in_month_label_abbr <- function(x){
   lubridate::month(x, label = TRUE, abbr = TRUE)
 }

 in_month_label_full <- function(x){
   lubridate::month(x, label = TRUE, abbr = FALSE)
 }
