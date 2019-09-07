# The following functions are deprecated.

also_occurring <- function(...){

  also_occur(...)
}

only_occurring <- function(...){

  only_occur(...)
}

not_occurring <- function(...){

  dont_occur(...)
}

is_occurring <- function(date, schedule){

  schedule(date)
}

occur <- function(schedule, date){

  schedule(date)
}

test_date <- function(date, schedule, ...){

  happen(schedule, date)
}

