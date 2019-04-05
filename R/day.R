on_mday <- function(x, ...){
  make_element(x, lubridate::mday, ...)
}

on_yday <- function(x, ...){
  make_element(x, lubridate::yday, ...)
}

on_qday <- function(x, ...){
  make_element(x, lubridate::qday, ...)
}

on_wday <- function(x, override_name_check = FALSE, ...){

  #if(override_name_check == FALSE){

    if(!(x %in% get_all_day_specs())){
      stop("x is not a legitimate day name")
    }

  #}

  if(x %in% 1:7){
    appro_function <- lubridate::wday}

  if(x %in% get_day_names()){
    appro_function <- on_wday_label_full}

  if(x %in% get_day_abbr_names()){
    appro_function <- on_wday_label_abbr}

  # make_element(x, list(lubridate::wday,
  #                      on_wday_label_abbr,
  #                      on_wday_label_full), ...)

  make_element(x, appro_function, ...)
}

on_wday_label_abbr <- function(x, ...){
  lubridate::wday(x, label = T, abbr = T, ...)
}

on_wday_label_full <- function(x, ...){
  lubridate::wday(x, label = T, abbr = F, ...)
}
