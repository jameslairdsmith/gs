#' @import magrittr readr
#' @importFrom magrittr %>%

get_month_names <- function(){
  # readr::locale() %>%
  #   magrittr::use_series(date_names) %>%
  #   magrittr::use_series(mon)
  locale()$date_names$mon
}

get_month_abbr_names <- function(){
  # readr::locale() %>%
  #   magrittr::use_series(date_names) %>%
  #   magrittr::use_series(mon_ab)
  locale()$date_names$mon_ab
}

get_all_month_specs <- function(){
  c(1:12,
    get_month_names(),
    get_month_abbr_names())
}

get_day_names <- function(){
  # readr::locale() %>%
  #   magrittr::use_series(date_names) %>%
  #   magrittr::use_series(day)
  locale()$date_names$day
}

get_day_abbr_names <- function(){
  # readr::locale() %>%
  #   magrittr::use_series(date_names) %>%
  #   magrittr::use_series(day_ab)
  locale()$date_names$day_ab
}

get_all_day_specs <- function(){
  c(1:7,
    get_day_names(),
    get_day_abbr_names())
}

