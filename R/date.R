on_date <- function(x, ...){

  out <- make_element(x, identity, ...)

  class(out) <- "date_element"

  out
}

# after <- function(x, within_given, ...){
#
#   if(is.character(within_given)){
#     within_given <- strings_to_date_functions(within_given)
#   }
#
#   output <- list(x = x,
#                  within_given = within_given)
#
#   class(output) <- "date_after_element"
#
#   output
# }
#
# before <- function(x, within_given, ...){
#
#   if(is.character(within_given)){
#     within_given <- strings_to_date_functions(within_given)
#   }
#
#   output <- list(x = x,
#                  within_given = within_given)
#
#   class(output) <- "date_before_element"
#
#   output
# }
