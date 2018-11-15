is_date_element <- function(x){
  inherits(x, "date_element")
}

is_schedule <- function(x){
  inherits(x, "schedule")
}

is_temporal <- function(x){
  is_date_element(x) | is_schedule(x)
}
