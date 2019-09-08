roll_forward <- function(schedule, to_following){

  #, n = 1, .p = NULL

  date_test <- function(date){

    are_candidate_dates <- happen(to_following, date)
  }

  out <- list(date_test = date_test)

  out$n_terms <- 1

  class(out) <- "schedule"

  out

}
