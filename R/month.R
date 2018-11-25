on_month <- function(x, label = TRUE, abbr = TRUE, ...){
  make_element(x, lubridate::month, label = label, abbr = abbr, ...)
}
