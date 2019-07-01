is_date_element <- function(x){
  inherits(x, "date_element")
}

is_date_range_element <- function(x){
  inherits(x, "date_range_element")
}

is_date_before_element <- function(x){
  inherits(x, "date_before_element")
}

is_date_after_element <- function(x){
  inherits(x, "date_after_element")
}

is_schedule <- function(x){
  inherits(x, "schedule")
}

is_temporal <- function(x){
  is_date_element(x) | is_schedule(x)
}

both_logical <- function(x){

  if(!is_schedule(x)){
    stop("Not a schedule object", call. = F)
  }

  if(length(x) != 2){
    stop("Not legitimate", call. = F)
  }

  is.logical(x[[1]]) & is.logical(x[[2]])
}

is_function_list <- function(x){

  x %>%
    purrr::map(rlang::is_function) %>%
    purrr::flatten_lgl() %>%
    all()
}

silent_equals_test <- function(x, y){
  suppressWarnings(magrittr::equals(x, y))
}

is_or_schedule <- function(x){
  inherits(x, "or_schedule")
}

is_and_schedule <- function(x){
  inherits(x, "and_schedule")
}

is_not_schedule <- function(x){
  inherits(x, "not_schedule")
}

or_schedule_early_recon <- function(or_schedule){
  if(!is_or_schedule){stop("Should only be applied to or_schedule object")}

  if(or_schedule[[1]] | or_schedule[[2]]){
    TRUE
  } else {or_schedule}
}

strings_to_date_functions <- function(str){
  str <- str
  .f <- dplyr::case_when(
    str == "month" ~ quote(lubridate::month),
    str == "quarter" ~ quote(lubridate::quarter),
    str == "year" ~ quote(lubridate::year),
    str == "semester" ~ quote(lubridate::semester)
  )
  eval(.f)
}

check_vec_loop <- function(vec, func, ...){
  if(length(vec) > 1){

    my_schedule <- func(vec[1], ...)

    for(i in 2:length(vec)){
      my_schedule <- also_occuring(my_schedule, func(vec[i], ...))
    }
    my_schedule
  }
}
