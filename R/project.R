project_dates <- function(x, ...){

  date_seq <- seq.Date(...)

  date_seq[date_seq(x, date_seq)]
}
