in_year <- function(...){

  x <- unlist(list(...))

  if(length(x) > 1) return(check_vec_loop(x, in_year))

  make_element(x, lubridate::year)
}

in_isoyear <- function(...){

  x <- unlist(list(...))

  if(length(x) > 1) return(check_vec_loop(x, in_isoyear))

  make_element(x, lubridate::isoyear)
}

in_epiyear <- function(...){

  x <- unlist(list(...))

  if(length(x) > 1) return(check_vec_loop(x, in_epiyear))

  make_element(x, lubridate::epiyear)
}
