#' fmt
#' 
#' Rounds and adds trailing zeros (by default if digits is lower than 4).
#' 
#' @param x numeric: vector.
#' @param digits integer: Digits that should be used for rounding.
#' @param zeros logical: Should trailing zeros be added?
#' @param ... passed to format for `fmt`.
#' 
#' @source Similar to function `fmt` from the package [exams](https://CRAN.R-project.org/package=exams). 
#' In the function `round2` has been replaced by `round`.
#' @seealso [exams::fmt]
#' @return formatted `x` as text.
#' @export
#' @md
#'
#' @examples
#' ## this is also employed internally in the fmt() formatting function
#' fmt(c(0.005, 0.015))
#' 
#' ## the main purpose of fmt() is that some numeric result can be displayed
#' ## both at high accuracy and then at the rounding that students should do
#' ## (e.g., with 2 or 3 digits)
#' sol <- runif(1)
#' fmt(sol, 6)
#' fmt(sol, 2)
#' 
#' ## but fmt() also assures showing a very high number of significant digits
#' ## (up to 12)
#' sol <- 123456 + sol
#' sol
#' fmt(sol, 6)
#' fmt(sol, 2)
#' 
#' ## and fmt() also takes care of adding trailing zeros (if digits < 4)
#' fmt(1)
#' fmt(1, digits = 3)
#' fmt(1, digits = 6)
fmt <- function(x, digits = 2L, zeros = digits < 4L, ...) {
  x <- round(x, digits = digits)
  if(zeros) {
    format(x, nsmall = digits, scientific = FALSE, digits = 12, ...)
  } else {
    format(x, scientific = FALSE, digits = 12, ...)
  }
}