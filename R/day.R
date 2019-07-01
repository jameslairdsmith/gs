on_mday <- function(x, ...){
  make_element(x, lubridate::mday, ...)
}

on_yday <- function(x, ...){
  make_element(x, lubridate::yday, ...)
}

on_qday <- function(x, ...){
  make_element(x, lubridate::qday, ...)
}

on_wday <- function(..., week_start = getOption("lubridate.week.start", 7)){

  x <- unlist(list(...))

  # if(length(x) > 1){
  #
  #   my_schedule <- on_wday(x[1], week_start = week_start)
  #
  #   for(i in 2:length(x)){
  #     my_schedule <- also_occuring(my_schedule,
  #                                  on_wday(x[i],
  #                                          week_start = week_start))
  #   }
  #   return(my_schedule)
  # }

  if(length(x) > 1){

    my_schedule <- check_vec_loop(x, on_wday, week_start = week_start)

    return(my_schedule)
  }


#  if(length(x) > 1){return(my_schedule)}

  if(!(x %in% get_all_day_specs())){
    stop("x is not a legitimate wday name")}

  if(x %in% 1:7){
    partial_wday <- purrr::partial(lubridate::wday, week_start = week_start)
    appro_function <- partial_wday}

  if(x %in% get_day_names()){
    appro_function <- on_wday_label_full}

  if(x %in% get_day_abbr_names()){
    appro_function <- on_wday_label_abbr}

  make_element(x, appro_function)

}

on_wday_label_abbr <- function(x, ...){
  lubridate::wday(x, label = T, abbr = T, ...)
}

on_wday_label_full <- function(x, ...){
  lubridate::wday(x, label = T, abbr = F, ...)
}

on_weekend <- function(){
  on_wday("Sat", "Sun")
}
