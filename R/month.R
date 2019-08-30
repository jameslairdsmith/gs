#' Specify the month(s) of a schedule
#'
#' Creates a schedule of events occurring in the months specified.
#'
#' Months can be specified using their names (eg. "January"),
#' abbreviations (eg. "Jan") or integers (eg. 1).
#'
#' Multiple months can be specified in the same function call. For example:
#' `in_month(1, 3)` produces a schedule of events occurring in January and
#' March. The same thing is achieved by `in_month("Jan", "Mar")`.
#' Similarly `in_month(1:3)` produces a schedule of events occurring in
#' January, February and March.
#'
#' @param ... A character or numeric vector of month elements.
#'
#' @keywords month, date, schedule
#' @return A schedule object.
#' @examples
#' my_dates <- seq.Date(as.Date("2000-01-01"),
#'                      as.Date("2000-04-01"),
#'                      by = "1 month")
#'
#' happen(in_month("January"), my_dates)
#'
#' happen(in_month("Feb"), my_dates)
#'
#' happen(in_month(3), my_dates)
#'
#' happen(in_month("Jan", "Mar"), my_dates)
#'
#' happen(in_month(1:3), my_dates)
#'
#' ## invalid inputs will produce an immediate error
#' \dontrun{
#' in_january <- in_month("Janu")
#' in_january <- in_month(0)}
#' @export


in_month <- function(...){

   x <- unlist(list(...))

   sch_print_method <- make_month_schedule_description(x)

   if(length(x) > 1) return(check_vec_loop(x,
                                           in_month,
                                           print_method = sch_print_method))

   if(!(x %in% get_all_month_specs())){
      stop("x is not a legitimate month name")}

   if(x %in% 1:12){
      appro_function <- lubridate::month}

   if(x %in% get_month_names()){
      appro_function <- in_month_label_full}

   if(x %in% get_month_abbr_names()){
      appro_function <- in_month_label_abbr}

   out <- make_element(x, appro_function)

   out$print_method <- sch_print_method

   out
}

in_month_label_abbr <- function(x){
   lubridate::month(x, label = TRUE, abbr = TRUE)
 }

in_month_label_full <- function(x){
   lubridate::month(x, label = TRUE, abbr = FALSE)
}

iden_func <- function(...){
   return(list(...))
}

make_month_abbr_for_printing <- function(x){

   month_numbers <- as.character(1:12)
   month_names <- get_month_names()
   month_abbr_names <- get_month_abbr_names()


   possible_values <-
      c(month_numbers,
        month_names,
        month_abbr_names)

   compare_table <- data.frame(possible_values,
                               month_abbr_names,
                               stringsAsFactors = F,
                               row.names = NULL)

   my_abbrs <- unique(compare_table[possible_values %in% x, "month_abbr_names"])

   as.character(sort(factor(my_abbrs, levels = month.abb)))
}

make_month_schedule_description <- function(x){

   my_sep_abbrs <- stringr::str_c(make_month_abbr_for_printing(x),
                                  collapse = ", ")
   stringr::str_c("- In months: ", my_sep_abbrs)
}
