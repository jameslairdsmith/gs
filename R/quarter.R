in_quarter <- function(...){

  x <- unlist(list(...))

  if(length(x) > 1) return(check_vec_loop(x, in_quarter))

  make_element(x, lubridate::quarter)

}

in_semester <- function(x){

  x

  make_element(x, lubridate::semester)

}
