schedule <- function(x, from, to, ...){

  if(is.numeric(from)){
    from <- lubridate::make_date(year = from)
  }

  if(is.numeric(to)){
    to <- lubridate::make_date(year = to, month = 12, day = 31)
  }

  date_seq <- seq.Date(from = from, to = to, by = "1 day")

  date_seq[test_date(date_seq, x)]
}
