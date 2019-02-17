# in_month <- function(x, label = TRUE, abbr = TRUE, ...){
#   make_element(x, list(lubridate::month,
#                        in_month_label_abbr,
#                        in_month_label_full), label = label, abbr = abbr, ...)
# }

in_month <- function(x, override_name_check = FALSE, ...){

   if(override_name_check == FALSE){

   if(!(x %in% get_all_month_specs())){
      stop("x is not a legitimate month name")
   }}

   make_element(x, list(lubridate::month,
                        in_month_label_abbr,
                        in_month_label_full), ...)
}


# month_plain <- function(x){
#   lubridate::month(x)
# }
#
 in_month_label_abbr <- function(x){
   lubridate::month(x, label = TRUE, abbr = TRUE)
 }
#
 in_month_label_full <- function(x){
   lubridate::month(x, label = TRUE, abbr = FALSE)
 }
