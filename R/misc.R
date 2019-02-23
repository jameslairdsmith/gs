is_date_element <- function(x){
  inherits(x, "date_element")
}

is_date_range_element <- function(x){
  inherits(x, "date_range_element")
}

is_date_before_element <- function(x){
  inherits(x, "date_before_element")
}

is_date_after_element <- function(x){
  inherits(x, "date_after_element")
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

is_or_schedule <- function(x){
  inherits(x, "or_schedule")
}

is_and_schedule <- function(x){
  inherits(x, "and_schedule")
}

or_schedule_early_recon <- function(or_schedule){
  if(!is_or_schedule){stop("Should only be applied to or_schedule object")}

  if(or_schedule[[1]] | or_schedule[[2]]){
    TRUE
  } else {or_schedule}
}

can_recon <- function(x){

  if(x %>% purrr::map_lgl(rlang::is_logical) %>% all()){
    return(TRUE)
  }

  if(is_and_schedule(x)){
     if(rlang::is_false(x[[1]]) | rlang::is_false(x[[2]])){return(TRUE)} else {return(FALSE)}
  }

  if(is_or_schedule(x)){
     if(rlang::is_true(x[[1]]) | rlang::is_true(x[[2]])){return(TRUE)} else {return(FALSE)}
  }
}
