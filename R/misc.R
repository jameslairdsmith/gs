is_date_element <- function(x){
  inherits(x, "date_element")
}

is_date_range_element <- function(x){
  inherits(x, "date_range_element")
}

is_schedule <- function(x){
  inherits(x, "schedule")
}

is_temporal <- function(x){
  is_date_element(x) | is_schedule(x)
}

both_logical <- function(x){

  if(!is_schedule(x)){
    stop("Not a schedule object", call. = F)
  }

  if(length(x) != 2){
    stop("Not legitimate", call. = F)
  }

  is.logical(x[[1]]) & is.logical(x[[2]])
}

is_function_list <- function(x){

  x %>%
    purrr::map(rlang::is_function) %>%
    purrr::flatten_lgl() %>%
    all()
}

silent_equals_test <- function(x, y){
  suppressWarnings(magrittr::equals(x, y))
}
