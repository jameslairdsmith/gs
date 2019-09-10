roll_by <- function(schedule, n, unit, .p = NULL){

  my_schedule <- schedule

  if(class(unit) == "character"){

    unit_freq <- lubridate::period(n, units = unit)

    date_test <- function(date){

      adjusted_date <- date %m-% unit_freq

      result <- happen(my_schedule, adjusted_date)

      if(!is.null(.p)){
        is_applicable <- happen(.p, adjusted_date)
        result <- result & is_applicable
        }

      result
      }

    }

  out <- list(date_test = date_test)

  out$n_terms <- 1

  class(out) <- "schedule"

  if(!is.null(.p)){
    applicable_schedule <- only_occur(my_schedule, dont_occur(.p))
    out <- also_occur(out, applicable_schedule)
  }

  out

}




