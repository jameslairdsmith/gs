project_dates <- function(x, from, to, ...){

  date_seq <- seq.Date(from = from, to = to, by = "1 day")

  date_seq[test_date(date_seq, x)]
}
