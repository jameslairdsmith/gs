## Consider how this should work with also_occuring() functions.

schedule <- function(x, from, to, during = NULL, ...){

  if("earliest_date" %in% get_attribute_names(x)){
    from <- attr(x, "earliest_date")
  }

  if("latest_date" %in% get_attribute_names(x)){
    to <- attr(x, "latest_date")
  }

  if(!is.null(during)){
    from <- lubridate::make_date(year = during)
    to <- lubridate::make_date(year = during, month = 12, day = 31)
  }

  if(is.numeric(from)){
    from <- lubridate::make_date(year = from)
  }

  if(is.numeric(to)){
    to <- lubridate::make_date(year = to, month = 12, day = 31)
  }

  date_seq <- seq.Date(from = from, to = to, by = "1 day")

  date_seq[test_date(date_seq, x)]
}


## This needs tests

schedule_hours <- function(x, from, to, during = NULL, ...){

  if(!is.null(during)){
    from <- lubridate::make_date(year = during)
    to <- lubridate::make_date(year = during, month = 12, day = 31)
  }

  if(is.numeric(from)){
    from <- lubridate::make_date(year = from)
  }

  if(is.numeric(to)){
    to <- lubridate::make_date(year = to, month = 12, day = 31)
  }

  to <- as_datetime(to)
  from <- as_datetime(from)

  datetime_seq <- seq.POSIXt(from = from, to = to, by = "1 hour")

  datetime_seq[test_date(datetime_seq, x)]
}
