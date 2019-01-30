get_month_names <- function(){
  readr::locale() %>%
    magrittr::use_series(date_names) %>%
    magrittr::use_series(mon)
}

get_month_abbr_names <- function(){
  readr::locale() %>%
    magrittr::use_series(date_names) %>%
    magrittr::use_series(mon_ab)
}

get_day_names <- function(){
  readr::locale() %>%
    magrittr::use_series(date_names) %>%
    magrittr::use_series(day)
}

get_day_abbr_names <- function(){
  readr::locale() %>%
    magrittr::use_series(date_names) %>%
    magrittr::use_series(day_ab)
}
