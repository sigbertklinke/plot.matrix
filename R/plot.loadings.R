#' plot.loadings
#'
#' Visualizes the loadings matrix from a Factor Analysis or a Principal Component Analysis matrix 
#' with a colored heatmap. You may need to modify \code{mar} with the \code{\link[graphics]{par}} 
#' command from its default \code{c(5.1,4.1,4.1,2.1)}.
#' See
#' \itemize{
#' \item \code{vignette('plot.matrix')} for detailed examples, and
#' \item \code{\link{plot.matrix}} for further parameters.
#' } 
#'
#' @param x matrix: loadings
#' @param reorder locgical: if the rows (variables) of the loading matrix should be reordered (default: \code{TRUE})
#' @param ... further parameter given to the \code{\link{plot.matrix}} command
#' 
#' @return a plot
#' @importFrom grDevices colorRampPalette
#' @export
#'
#' @examples
#' data(bfi.2)
#' library("psych")
#' par(mar=c(5.1, 4.1, 4.1, 4.1))
#' # Factor analysis
#' fa <- factanal(bfi.2, 5)
#' plot(loadings(fa))
#' # Principal Component Analysis I
#' pa <- princomp(bfi.2)
#' plot(loadings(pa), digits=NA)
#' # Principal Component Analysis II
#' pa <- prcomp(bfi.2)
#' ld <- structure(pa$rotation, class="loadings") 
#' plot(ld, digits=NA)
plot.loadings <- function(x, reorder = TRUE, ...) {
  args <- list(...)
  if (is.null(args$main)) args$main <- paste(deparse(substitute(x)), collapse = "\n")
  if (is.null(args$breaks)) args$breaks <- c(-sqrt(c(1, 0.75, 0.5, 0.25, 0.16)), sqrt(c(0.16, 0.25, 0.50, 0.75, 1)))
  if (is.null(args$digits)) args$digits <- 2
  if (is.null(args$xlab)) args$xlab <- "Component"
  if (is.null(args$ylab)) args$ylab <- "Variable"
  args$x <- unclass(x)
  if (is.null(args$col))args$col <- colorRampPalette(c("blue", "white", "red"))
  if (is.null(rownames(args$x))) rownames(args$x) <- sprintf("V%.0f", 1:nrow(x))
  if (reorder) {
    fi <- matrix(findInterval(abs(args$x), args$breaks), ncol=ncol(args$x))
    l <- c(split(fi,  rep(1:ncol(fi), each = nrow(fi))), split(abs(args$x),  rep(1:ncol(args$x), each = nrow(args$x))))
    o  <- do.call('order', l)
    args$x <- args$x[rev(o),]
  }
  do.call("plot.matrix", args)
}