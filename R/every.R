on_every_n <- function(n, unit, starting, inclusive = T, backdated = F){

  if(class(unit) == "function"){

    unit_freq <- unit(n)

      date_test <- function(date){

        my_interval <- interval(starting, date)

        period_period <- as.period(my_interval) / unit_freq

        interval_period <- my_interval / unit_freq

        result <- is_whole_number(period_period) | is_whole_number(interval_period)

        if(inclusive == F){
          result[period_period == 0] <- FALSE
          result[interval_period == 0] <- FALSE
        }

        if(backdated == F){
          result[period_period < 0] <- FALSE
          result[interval_period < 0] <- FALSE
        }

        result

      }

      out <- list(date_test = date_test)

      class(out) <- "schedule"

      out$terms <- 1

      out
  }


}

on_every <- function(unit, starting, inclusive = T, backdated = F){
  every_n(1,
          unit = unit,
          starting = starting,
          inclusive = inclusive,
          backdated = backdated)
}
