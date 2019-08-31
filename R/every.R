every_n <- function(n, unit, since){

  unit_freq <- unit(n)

  if(class(unit_freq) == "Period"){

    date_test <- function(date){

      between_dates <- as.period(date - since)
      is_whole_number(between_dates / unit_freq)
    }

    out <- list(date_test = date_test)

    class(out) <- "schedule"

    out$terms <- 1

    out
  }
}

every <- function(unit, since){
  every_n(1, unit = unit, since = since)
}
