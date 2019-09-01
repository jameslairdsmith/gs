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

      return(out)
  }

  if(class(unit) == "schedule"){

    if(!happen(unit, starting)){stop("starting date isn't on schedule")}

    date_test <- function(date){

      compiled_df <-
        tibble::tibble(candidate_dates = date) %>%
        dplyr::mutate(result = purrr::map(candidate_dates,
                                          schedule_days,
                                          x = unit,
                                          from = starting)) %>%
        dplyr::mutate(result = purrr::map(result, remove_first)) %>%
        dplyr::mutate(num_dates = purrr::map_int(result, length)) %>%
        dplyr::mutate(num_dates = num_dates /n) %>%
        dplyr::filter(is_whole_number(num_dates),
                      happen(unit, candidate_dates))

      applicable_dates <- compiled_df[["candidate_dates"]]

      result <- date %in% applicable_dates

      if(inclusive == FALSE){
        result[date == starting] <- FALSE
      }

      if(backdated == FALSE){
        result[date < starting] <- FALSE
      }

      result
    }

    out <- list(date_test = date_test)

    class(out) <- "schedule"

    out$n_terms <- 1

    return(out)
  }

}

on_every <- function(unit, starting, inclusive = T, backdated = F){
  every_n(1,
          unit = unit,
          starting = starting,
          inclusive = inclusive,
          backdated = backdated)
}

