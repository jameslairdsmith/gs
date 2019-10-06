#' Roll scheduled events
#'
#' @description
#' Given a schedule of events, create a new schedule where the events are
#' adjusted according to some rule.
#'
#' * `roll_by()` adjusts the events of a schedule by some incremental period.
#' * `roll_forward()` and `roll_backward()` adjust the events of a schedule
#' to the nth previous/next occurrence of some other scheduled event.
#'
#' @param x The schedule of events to adjust.
#' @param n The increment of the event adjustment.
#'   * For `roll_by()` this is the increment of the `unit` period to adjust the
#'   events by. Eg. `2` to adjust the events two `unit` periods into the future
#'    or `-3` to adjust the events three unit periods into the past.
#'   * For `roll_forward()` and `roll_backward()`, `n` defaults to 1, indicating
#'   the schedule should be adjusted to the `to_schedule` event immediately
#'   preceding/following the events of `x`. If more than 1, will skip n-1
#'   occurrences.
#' @param unit  A character shortcut for a period object. Eg. "year", "years",
#'    "month", "months", "week", "weeks" etc. Can be any value accepted by
#'    `lubridate::period()`.
#' @param to_schedule A schedule to which events can be rolled.
#' @param .p Optionally, a schedule to use for limiting the adjustment
#' performed on `x`. Events falling on `.p` will be adjusted. Events not
#' falling on `.p` will be returned unadjusted in the output schedule. Leave
#' `NULL` (the default) to adjust all the events of `x`.
#' * Eg. `roll_forward(x, to_schedule = on_wday("Sun"))` rolls the events of `x`
#' to the next Sunday.
#'
#' @return A schedule object.
#' @examples
#'
#' library(lubridate, warn.conflicts = FALSE)
#' library(magrittr, warn.conflicts = FALSE)
#'
#' # Imagine you get paid on the 25th of the month
#'
#' on_payday <- on_mday(25)
#'
#' schedule_days(on_payday, during = 2000)
#'
#' # Except if your payday falls on a weekend, in which case it moves to the
#' # next weekday
#'
#' on_payday %>%
#'   roll_forward(to = on_weekday(), .p = on_weekend()) %>%
#'   schedule_days(during = 2000)
#'
#' # For some people payday may adjust to the previous weekday if it falls on
#' # a weekend
#'
#' on_payday %>%
#'   roll_backward(to = on_weekday(), .p = on_weekend()) %>%
#'   schedule_days(during = 2000)
#'
#' # Imagine the garbage truck normally comes every Monday, but if
#' # Monday is a state holiday, then it comes on Tuesday instead.
#'
#' on_labor_day <-
#'   on_first(on_wday("Mon"), within_given = "month") %>%
#'   only_occur(in_month("Sep"))
#'
#' on_christmas_day <- only_occur(in_month("Dec"), on_mday(25))
#'
#' on_non_working_day <-
#'   on_weekend() %>%
#'   also_occur(on_labor_day) %>%
#'   also_occur(on_christmas_day)
#'
#' on_my_business_day <- dont_occur(on_non_working_day)
#'
#' on_normal_trash_day <- on_wday("Mon")
#'
#' on_trash_day <-
#'   on_normal_trash_day %>%
#'   roll_forward(to_schedule = on_my_business_day, .p = on_non_working_day)
#'
#' # A Monday in September
#' happen(on_normal_trash_day, ymd("2019-09-09"))
#' happen(on_trash_day, ymd("2019-09-09"))
#'
#' # Labor Day Monday should not be trash day
#' happen(on_normal_trash_day, ymd("2019-09-02"))
#' happen(on_trash_day, ymd("2019-09-02"))
#'
#' # The day after Labor Day Monday is trash day
#' happen(on_normal_trash_day, ymd("2019-09-03"))
#' happen(on_trash_day, ymd("2019-09-03"))
#'
#' # Say that a trash inspector always comes the day after trash day, whatever
#' # day that happens to be.
#'
#' on_inspection_day <-
#'   on_trash_day %>%
#'   roll_by(1, "day")
#'
#' # Inspector comes on a Tuesday in September
#' happen(on_inspection_day, ymd("2019-09-10"))
#'
#' # Inspector doesn't come on that Wednesday
#' happen(on_inspection_day, ymd("2019-09-11"))
#'
#' # Inspector doesn't come on the Tuesday after Labor Day Monday
#' happen(on_inspection_day, ymd("2019-09-03"))
#'
#' # Inspector does come on that Wednesday
#' happen(on_inspection_day, ymd("2019-09-04"))
#' @export

roll_by <- function(x, n, unit, .p = NULL){

  my_schedule <- x

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

  if("latest_date" %in% get_attribute_names(x)){
    attr(out, "latest_date") <- attr(x, "latest_date")
  }
  if("earliest_date" %in% get_attribute_names(x)){
    attr(out, "earliest_date") <- attr(x, "earliest_date")
  }

  out

}

#' @rdname roll_by
#' @export

roll_forward <- function(x, to_schedule, n = 1, .p = NULL){

  my_schedule <- x

  date_test <- function(date){

    candidate_dates <- date

    rolling_dates <- candidate_dates

    need_rolling <- !happen(my_schedule, candidate_dates)

    n_counter <- rep_len(0L, length(candidate_dates))

    while(any(need_rolling)){

      is_applicable <- happen(to_schedule, rolling_dates)

      n_counter[need_rolling & is_applicable] <-
        n_counter[need_rolling & is_applicable] + 1L

      rolling_dates[need_rolling] <- rolling_dates[need_rolling] %m-% days(1)

      need_rolling <- !happen(my_schedule, rolling_dates)

    }

    on_target_candidates <- happen(to_schedule, candidate_dates)
    target_counter <- n_counter == n

    is_successful_candidate <- on_target_candidates & target_counter

    successful_candidates <- candidate_dates[is_successful_candidate]

    result <- date %in% successful_candidates

    if(!is.null(.p)){
      is_applicable <- happen(.p, rolling_dates)
      result <- result & is_applicable
    }

    result
  }

  out <- list(date_test = date_test)

  out$n_terms <- 1

  class(out) <- "schedule"

  if(!is.null(.p)){
    applicable_schedule <- only_occur(my_schedule, dont_occur(.p))
    out <- also_occur(out, applicable_schedule)
  }

  if("latest_date" %in% get_attribute_names(x)){
    attr(out, "latest_date") <- attr(x, "latest_date")
  }
  if("earliest_date" %in% get_attribute_names(x)){
    attr(out, "earliest_date") <- attr(x, "earliest_date")
  }


  out

}

#' @rdname roll_by
#' @export

roll_backward <- function(x, to_schedule, n = 1, .p = NULL){

  my_schedule <- x

  date_test <- function(date){

    candidate_dates <- date

    rolling_dates <- candidate_dates

    need_rolling <- !happen(my_schedule, candidate_dates)

    n_counter <- rep_len(0L, length(candidate_dates))

    while(any(need_rolling)){

      is_applicable <- happen(to_schedule, rolling_dates)

      n_counter[need_rolling & is_applicable] <-
        n_counter[need_rolling & is_applicable] + 1L

      rolling_dates[need_rolling] <- rolling_dates[need_rolling] %m+% days(1)

      need_rolling <- !happen(my_schedule, rolling_dates)

    }

    on_target_candidates <- happen(to_schedule, candidate_dates)
    target_counter <- n_counter == n

    is_successful_candidate <- on_target_candidates & target_counter

    successful_candidates <- candidate_dates[is_successful_candidate]

    result <- date %in% successful_candidates

    if(!is.null(.p)){
      is_applicable <- happen(.p, rolling_dates)
      result <- result & is_applicable
    }

    result
  }

  out <- list(date_test = date_test)

  out$n_terms <- 1

  class(out) <- "schedule"

  if(!is.null(.p)){
    applicable_schedule <- only_occur(my_schedule, dont_occur(.p))
    out <- also_occur(out, applicable_schedule)
  }

  if("latest_date" %in% get_attribute_names(x)){
    attr(out, "latest_date") <- attr(x, "latest_date")
  }
  if("earliest_date" %in% get_attribute_names(x)){
    attr(out, "earliest_date") <- attr(x, "earliest_date")
  }

  out

}




