schedule_annually <- function(starting, ending, years, inclusive = TRUE){
  #if(missing(ending)){starting %m+% years(0:years)}

  if(inclusive == FALSE){
    num_start <- 1
  } else {
    num_start <- 0
  }

  if(missing(ending)){
      result <- starting %m+% years(num_start:years)}

  if(missing(starting)){
      result <- ending %m-% years(num_start:years)
  }
  sort(result)
}
