in_month <- function(x, label = TRUE, abbr = TRUE, ...){
  make_element(x, list(lubridate::month,
                       in_month_label_abbr,
                       in_month_label_full), label = label, abbr = abbr, ...)
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
