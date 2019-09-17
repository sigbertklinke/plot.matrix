#' @rdname as.cor
#' @name as.cor
#' @aliases as.assoc
#' @aliases as.pvalue
#' @title as.XXX conversion functions
#' @param x numeric matrix: matrix to convert
#'
#' @export
#' @return a matrix with an appropriate class
#'
#' @examples
#' # as.cor
#' c <- cor(airquality, use="complete.obs")
#' # as.assoc
#' # as.pvalue
#' data(air.pvalue)
#' plot(as.pvalue(air.pvalue))
as.cor    <- function(x) { class(x) <- c("cor", class(x)); x }

#' @export
#' @rdname as.cor
as.assoc  <- function(x) { class(x) <- c("assoc", class(x)); x }

#' @export
#' @rdname as.cor
as.pvalue <- function(x) { class(x) <- c("pvalue", class(x)); x }