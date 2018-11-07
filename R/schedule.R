#' @rdname schedule
#' @export

schedule <- function(object, ...)
  UseMethod("schedule")

schedule.default <- function(){

  out <- list(
    elements = list()
  )

  class(out) <- "schedule"

  out
}
