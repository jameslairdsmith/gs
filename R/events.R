get_events <- function(x,
                       n = NULL,
                       since = NULL,
                       until = NULL,
                       during = NULL,
                       every = "day",
                       every_n = 1,
                       limit = lubridate::years(10)){

  from <- get_from(x = x, from = since, during = during)
  to <- get_to(x = x, to = until, during = during)

  if(is.null(to) & !is.null(n)){
    to <- from + limit
  }

  #if(!is.null(n)) return(get_n_events(x, n, start_date = from))

  date_seq <- make_period_seq(start = from,
                              end = to,
                              period_unit = every,
                              period_n = every_n)

  result <- date_seq[happen(x, date_seq)]

  if(!is.null(n)){
    result <- head(result, n)
  }

  result
}




get_n_events <- function(x, n, start_date, limit = lubridate::years(10)){

  on_my_schedule <- x
  changing_date <- start_date
  date_vector <- integer(0)
  class(date_vector) <- "Date"
  period_length <- limit
  end_limit <- start_date + period_length
  n_limit <- n
  n <- 1

  if(happen(on_my_schedule, start_date)){
    date_vector <- c(date_vector, start_date)
    n <- n + 1
  }

  while(n <= n_limit & changing_date < end_limit){

    changing_date <- changing_date + days(1)

    if(happen(on_my_schedule, changing_date)){
      n <- n + 1
      date_vector <- c(changing_date, date_vector)
    }
  }

  sort(date_vector)
}
