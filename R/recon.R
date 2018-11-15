recon <- function(object, ...)
  UseMethod("recon")

recon.or_schedule <- function(x){

  x[[1]] | x[[2]]
}

recon.and_schedule <- function(x){

  x[[1]] & x[[2]]
}
