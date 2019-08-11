#' Get the events of a schedule
#'
#' Get the events from a schedule object.
#'
#' @param ... A numeric vector of year elements.
#'
#' @keywords schedule
#' @return A date vector
#' @details R
#' @examples
#' on_first_day_month <- on_mday(1)
#'
#' schedule(on_first_day_month, during = 2000)
#' @export

schedule <- function(x, from = NULL, to = NULL, during = NULL, every = "1 day",...){

  from <- get_from(x = x, from = from, during = during)
  to <- get_to(x = x, to = to, during = during)

  date_seq <- seq.Date(from = from, to = to, by = every)

  date_seq[test_date(date_seq, x)]
}

#' @rdname schedule
#' @export

schedule_days <- function(x, from = NULL, to = NULL, during = NULL, ...){

  schedule(x = x, from = from, to = to, during = during, every = "1 day", ...)
}

#' @rdname schedule
#' @export

schedule_hours <- function(x, from = NULL, to = NULL, during = NULL, ...){

  from <- get_from(x = x, from = from, during = during)
  to <- get_to(x = x, to = to, during = during)

  to <- as_datetime(to)
  from <- as_datetime(from)

  datetime_seq <- seq.POSIXt(from = from, to = to, by = "1 hour")

  datetime_seq[test_date(datetime_seq, x)]
}

get_from <- function(x, from, during){

  if("earliest_date" %in% get_attribute_names(x)){
    return_from <- attr(x, "earliest_date")
  }

  if(!is.null(during)){
    return_from <- lubridate::make_date(year = during)
  }

  if(is.numeric(from)){
    return_from <- lubridate::make_date(year = from)
  }

  if(lubridate::is.Date(from)){
    return_from <- from
  }

  if(exists("return_from")){return(return_from)}else{return(NULL)}
}

get_to <- function(x, to, during){

  if("latest_date" %in% get_attribute_names(x)){
    return_to <- attr(x, "latest_date")
  }

  if(!is.null(during)){
    return_to <- lubridate::make_date(year = during, month = 12, day = 31)
  }

  if(is.numeric(to)){
    return_to <- lubridate::make_date(year = to, month = 12, day = 31)
  }

  if(lubridate::is.Date(to)){
    return_to <- to
  }

  if(exists("return_to")){return(return_to)}else{return(NULL)}
}
