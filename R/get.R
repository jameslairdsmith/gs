get_from <- function(x, from, during){

  if("earliest_date" %in% get_attribute_names(x)){
    return(attr(x, "earliest_date"))
  }

  if(lubridate::is.Date(from)){
    return(from)
  }

  if(is.numeric(from)){
    return(lubridate::make_date(year = from))
  }

  if(!is.null(during)){
    return(lubridate::make_date(year = during))
  }

  NULL
}

get_to <- function(x, to, during){

  if("latest_date" %in% get_attribute_names(x)){
    return(attr(x, "latest_date"))
  }

  if(lubridate::is.Date(to)){
    return(to)
  }

  if(is.numeric(to)){
    return(lubridate::make_date(year = to, month = 12, day = 31))
  }

  if(!is.null(during)){
    return(lubridate::make_date(year = during, month = 12, day = 31))
  }
}
