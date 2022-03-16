#' plot.cor
#'
#' Visualizes a correlation matrix with a colored or gray heatmap. As a rule of thumb the breaks are determined 
#' by the \href{https://en.wikipedia.org/wiki/Effect_size#Pearson_r_or_correlation_coefficient}{effect sizes} given by 
#' Cohen (\code{c(-1, -0.4, -0.2, -0.05, 0, +0.05, +0.2, +0.4, +1)}. 
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
#' @param x matrix: correlation within [-1,+1]
#' @param reorder logical: if the rows (variables) of the loading matrix should be reordered (default: \code{TRUE})
#' @param grey logical: should be a gray scale color palette used or not (default: \code{FALSE})
#' @param gray logical: should be a gray scale color palette used or not (default: \code{FALSE})
#' @param ... further parameter given to the \code{\link{plot.matrix}} command
#' 
#' @return a plot
#' @importFrom grDevices colorRampPalette
#' @importFrom stats hclust dist
#' @export 
#' @method plot cor 
#'
#' @examples
#' par(mar=c(5.1, 4.1, 4.1, 4.1))
#' # correlation matrix
#' c <- cor(airquality[,1:4], use="pairwise")
#' plot(as.cor(c))
#' plot(as.cor(c), gray=TRUE)
#' plot(as.cor(c[,1:3]), reorder=FALSE)
plot.cor <- function(x, reorder = TRUE, gray=FALSE, grey=FALSE, ...) {
  args <- list(...)
  if (is.null(args$main)) args$main <- paste(deparse(substitute(x)), collapse = "\n")
  if (is.null(args$breaks)) args$breaks <- c(-c(1, 0.4, 0.2, 0.05), c(0.05, 0.2, 0.4, 1))
  if (is.null(args$digits)) args$digits <- 2
  args$x <- unclass(x)
  if (is.null(args$col)) {
    args$col <- if (grey||gray) colorRampPalette(c("black", "white", "black")) else colorRampPalette(c("blue", "white", "red"))
  }
  if (is.null(rownames(args$x))) rownames(args$x) <- sprintf("V%.0f", 1:nrow(x))
  if (reorder) {
    o  <- hclust(dist(args$x))$order
    rn <- rownames(x)
    cn <- colnames(x)
    args$x <- if ((length(cn)==length(rn)) && all(rn==cn)) args$x[o,o,drop=FALSE] else args$x[o,,drop=FALSE]
  }
  do.call("plot.matrix", args)
}