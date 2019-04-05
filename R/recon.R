recon <- function(object, ...)
  UseMethod("recon")

recon.or_schedule <- function(x){

  #x[[1]] | x[[2]]


  if(rlang::is_true(x[[1]]) | rlang::is_true(x[[2]])){return(TRUE)} else {return(FALSE)}

}

recon.and_schedule <- function(x){

  #x[[1]] & x[[2]]

  if(rlang::is_false(x[[1]]) | rlang::is_false(x[[2]])){return(FALSE)} else {return(TRUE)}
  #if(x[[1]] == FALSE | x[[2]] == FALSE){return(FALSE)} else {return(TRUE)}
}

# recon.not_schedule <- function(x){
#
#   !x[[1]]
# }

can_recon <- function(x){



  if(is_and_schedule(x)){
    if(rlang::is_false(x[[1]]) | rlang::is_false(x[[2]])){return(TRUE)} #else {return(FALSE)}
  }

  if(is_or_schedule(x)){
    if(rlang::is_true(x[[1]]) | rlang::is_true(x[[2]])){return(TRUE)} #else {return(FALSE)}
  }

  if(x %>% purrr::map_lgl(rlang::is_logical) %>% all()){
    return(TRUE)
  }
  FALSE

}
