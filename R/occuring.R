also_occuring <- function(elem_1, elem_2){

  envoke_list <- list(elem_2, elem_1)

  out <- function(date){
    elem_1(date) | elem_2(date)
  }

  class(out) <- "schedule"

  out

}

only_occuring <- function(elem_1, elem_2){

  envoke_list <- list(elem_2, elem_1)

  out <- function(date){
    elem_1(date) & elem_2(date)
  }

  class(out) <- "schedule"

  if("latest_date" %in% get_attribute_names(elem_1)){
    attr(out, "latest_date") <- attr(elem_1, "latest_date")
  }
  if("earliest_date" %in% get_attribute_names(elem_1)){
    attr(out, "earliest_date") <- attr(elem_1, "earliest_date")
  }
  if("latest_date" %in% get_attribute_names(elem_2)){
    attr(out, "latest_date") <- attr(elem_2, "latest_date")
  }
  if("earliest_date" %in% get_attribute_names(elem_2)){
    attr(out, "earliest_date") <- attr(elem_2, "earliest_date")
  }

  out
}

not_occuring <- function(elem_1, elem_2 = NULL){

  if(is.null(elem_2)){

    out <- function(date){
      !elem_1(date)
    }

    class(out) <- c("schedule")

    out

  } else {

    out <- function(date){
      !elem_2(date)
    }

    class(out) <- c("schedule")

    elem_1 %>% only_occuring(out)
  }
}
