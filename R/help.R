# Always sorts results
# Does it make sense to have default values?
# It always adjusts for months
# Correct argument names : num_*
# Add skips; will also have impact for quarterly
# Write tests

schedule_periodically <- function(period_func,
                                  starting,
                                  ending,
                                  n,
                                  inclusive = TRUE,
                                  multi = 1,
                                  skips = NULL){

  if(inclusive == FALSE){
    num_start <- 1
  } else {
    num_start <- 0
  }

  if(missing(ending)){
    result <- starting %m+% period_func(multi * num_start:n)}

  if(missing(starting)){
    result <- ending %m-% period_func(multi * num_start:n)
  }
  sort(result)
}

# if(!is.null(skips)){
#
#   vec <- vector(mode = "logical", length = skips)
#
#
#   vec[length(vec)] <- TRUE
#
#   sort(result)[vec]
# }

schedule_annually <- function(starting, ending, years, inclusive = TRUE){
  schedule_periodically(lubridate::years, starting, ending, years, inclusive)
}

schedule_monthly <- function(starting, ending, months, inclusive = TRUE){
  schedule_periodically(lubridate:::months.numeric, starting, ending, months, inclusive)
}

schedule_quarterly <- function(starting, ending, quarters, inclusive = TRUE){

  schedule_periodically(lubridate:::months.numeric, starting, ending, quarters, inclusive, 3)
}
