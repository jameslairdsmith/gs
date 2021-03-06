strings_to_date_functions <- function(str){
  str <- str
  .f <- dplyr::case_when(
    str == "day" ~ quote(lubridate::day),
    str == "week" ~ quote(lubridate::week),
    str == "month" ~ quote(lubridate::month),
    str == "quarter" ~ quote(lubridate::quarter),
    str == "semester" ~ quote(lubridate::semester),
    str == "year" ~ quote(lubridate::year),
    str == "days" ~ quote(lubridate::day),
    str == "weeks" ~ quote(lubridate::week),
    str == "months" ~ quote(lubridate::month),
    str == "quarters" ~ quote(lubridate::quarter),
    str == "semesters" ~ quote(lubridate::semester),
    str == "years" ~ quote(lubridate::year)
  )
  eval(.f)
}

check_vec_loop <- function(vec, func, ...){

  if(length(vec) > 1){

    my_schedule <- func(vec[1], ...)

    for(i in 2:length(vec)){
      my_schedule <- also_occur(my_schedule, func(vec[i], ...))
    }
    my_schedule
  }
}

get_attribute_names <- function(obj){
  names(attributes(obj))
}

is_whole_number <- function(x){
  (x%%1==0)
}

remove_first <- function(vec){
  vec[-1]
}
