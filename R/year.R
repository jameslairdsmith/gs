


in_year <- function(...){

  x <- unlist(list(...))

  if(length(x) > 1){

    final_output <- check_vec_loop(x, in_year)

    earliest_date <- lubridate::make_date(year = min(x), month = 1, day = 1)
    latest_date <- lubridate::make_date(year = max(x), month = 12, day = 31)

    attr(final_output, "earliest_date") <- earliest_date
    attr(final_output, "latest_date") <- latest_date

    return(final_output)
    }

  output <- make_element(x, lubridate::year)

  earliest_date <- lubridate::make_date(year = min(x), month = 1, day = 1)
  latest_date <- lubridate::make_date(year = max(x), month = 12, day = 31)

  attr(output, "earliest_date") <- earliest_date
  attr(output, "latest_date") <- latest_date

  output
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
