make_element <- function(x, .f, ...){

  date_test <- function(date){
    .f(date, ...) == x
  }

  out <- list(date_test = date_test)

  class(out) <- "schedule"

  out
}


print.schedule <- function(x){
  cat("A schedule of events occuring:")
  cat("\n")
  for(i in 1:length(x$print_method)){
    cat(" ")
    cat(x$print_method[i], sep = "\n   ")
  }
}
