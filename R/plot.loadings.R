#' plot.loadings
#'
#' Visualizes the loadings matrix from a Factor Analysis or a Principal Component Analysis matrix 
#' with a gray or colored heatmap. As a rule of thumb the breaks are determined by 
#' \code{c(-1, -0.866, -0.707, -0.5, -0.4, 0, +0.4, +0.5, +0.707, +0.866, +1)} is used.
#' You may need to modify \code{mar} with the \code{\link[graphics]{par}} command from its default 
#' \code{c(5.1,4.1,4.1,2.1)}.
#' See
#' \itemize{
#' \item \code{vignette('plot.matrix')} for detailed examples, and
#' \item \code{\link{plot.matrix}} for further parameters.
#' } 
#'
#' @details If either the parameter \code{grey} or \code{gray} is \code{TRUE} then a gray color palette is used.
#'
#' @param x matrix: loadings
#' @param reorder logical: if the rows (variables) of the loading matrix should be reordered (default: \code{TRUE})
#' @param grey logical: should be a gray scale color palette used or not (default: \code{FALSE})
#' @param gray logical: should be a gray scale color palette used or not (default: \code{FALSE})
#' @param ... further parameter given to the \code{\link{plot.matrix}} command
#' 
#' @return a plot
#' @importFrom grDevices colorRampPalette
#' @export 
#' @method plot loadings
#'
#' @examples
#' data(bfi.2)
#' library("psych")
#' par(mar=c(5.1, 4.1, 4.1, 4.1))
#' # Factor analysis
#' fa <- factanal(bfi.2, 5)
#' plot(loadings(fa))
#' plot(loadings(fa), grey=TRUE)
#' # Principal Component Analysis I
#' pa <- princomp(bfi.2)
#' plot(loadings(pa), digits=NA)
#' # Principal Component Analysis II
#' pa <- prcomp(bfi.2)
#' ld <- structure(pa$rotation, class="loadings") 
#' plot(ld, digits=NA)
plot.loadings <- function(x, reorder = TRUE, gray=FALSE, grey=FALSE, ...) {
  args <- list(...)
  if (is.null(args$main)) args$main <- paste(deparse(substitute(x)), collapse = "\n")
  if (is.null(args$breaks)) args$breaks <- c(-sqrt(c(1, 0.75, 0.5, 0.25, 0.16)), sqrt(c(0.16, 0.25, 0.50, 0.75, 1)))
  if (is.null(args$digits)) args$digits <- 2
  if (is.null(args$xlab)) args$xlab <- "Component"
  if (is.null(args$ylab)) args$ylab <- "Variable"
  args$x <- unclass(x)
  if (is.null(args$col)) {
    args$col <- if (grey||gray) colorRampPalette(c("black", "white", "black")) else colorRampPalette(c("blue", "white", "red"))
  }
  if (is.null(rownames(args$x))) rownames(args$x) <- sprintf("V%.0f", 1:nrow(x))
  if (reorder) {
    #   till 1.3.1.
    #   fi <- matrix(findInterval(abs(args$x), args$breaks), ncol=ncol(args$x))
    #   l <- c(split(fi,  rep(1:ncol(fi), each = nrow(fi))), split(abs(args$x),  rep(1:ncol(args$x), each = nrow(args$x))))
    #   o  <- do.call('order', l)
    #   since 1.4
    o <- apply(x, 1, function(v) { v<-v^2; c(which.max(v), -max(v))})
    o <- order(o[1,], o[2,])
    args$x <- args$x[o,]
  }
  do.call("plot.matrix", args)
}