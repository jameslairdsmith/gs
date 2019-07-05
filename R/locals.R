#'  @import magrittr
#'  @importFrom magrittr %>%

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

get_all_month_specs <- function(){
  c(1:12,
    get_month_names(),
    get_month_abbr_names())
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

get_all_day_specs <- function(){
  c(1:7,
    get_day_names(),
    get_day_abbr_names())
}

