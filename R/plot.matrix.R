#' plot.matrix
#'
#' Visualizes a matrix with a heatmap and distinguishes between numeric and non-numeric matrices. 
#' You may need to modify \code{mar} with the \code{\link[graphics]{par}} command from its default \code{c(5.1,4.1,4.1,2.1)}. 
#'
#' In case of a numeric matrix it should hold \code{length(breaks)==length(col)+1}. If not or not given at all then the 
#' \code{breaks} is recalculated as an equidistant grid between \code{min(breaks)} and \code{max(breaks)}.
#' If \code{col} is not given then \code{heat.color(10)} is used.
#' 
#' In case of a non-numeric matrix it should hold \code{length(breaks)==length(col)}. If  not given at allthen the 
#' \code{breaks} is determined by the unique elements of the matrix. If \code{col} is not given then \code{heat.color} is used
#' with the number of unique elements in \code{x}.
#' 
#' @param x matrix
#' @param y unused
#' @param digits number of digits for numeric data or length of string for non-numeric data 
#' @param col a vector of colors
#' @param breaks breaks for numeric values or values for \code{col}
#' @param key list of parameters used for \code{\link[graphics]{axis}}. If set to \code{NULL} then no information will be plotted. 
#' @param na.col color for missing value (default: white)
#' @param ... further parameter given to the \code{\link[graphics]{plot}} command
#'
#' @return a plot
#' @importFrom grDevices heat.colors
#' @importFrom graphics axis polygon text
#' @export
#'
#' @aliases plot
#' @examples
#' par(mar=c(5.1, 4.1, 4.1, 4.1))
#' # numeric matrix
#' x <- matrix(runif(50), nrow=10)
#' plot(x)
#' plot(x, key=NULL)
#' plot(x, key=list(cex.axis=0.5, tick=FALSE))
#' plot(x, digits=3)
#' plot(x, breaks=c(0,1), digits=3, cex=0.6)
#' # logical matrix
#' m <- matrix(runif(50)<0.5, nrow=10)
#' plot(m)
#' plot(m, key=NULL, digits=1)
#' # character matrix
#' s <- matrix(sample(letters[1:10], 50, replace=TRUE), nrow=10)
#' plot(s)
#' plot(s, digits=10)
#' plot(s, digits=10, col=heat.colors(5), breaks=letters[1:5])
#' 
plot.matrix <- function(x, y=NULL, digits=NA, col=NULL, breaks=NULL, key=list(cex.axis=1), na.col="white", ...) {
  main     <- paste(deparse(substitute(x)), collapse = "\n")
  # type
  detect <- 0
  if ('numeric' %in% class(x[1,1])) {
    detect <- 1
    if (is.null(col)) col <- heat.colors(10)
    if (is.null(breaks)) {
      breaks <- seq(from=min(x, na.rm=TRUE), to=max(x, na.rm=TRUE), length.out=length(col)+1)
    } else {
      if ((length(breaks)+1)!=length(col)) 
        breaks <-seq(from=min(breaks, na.rm=TRUE), to=max(breaks, na.rm=TRUE), length.out=length(col)+1)
    }
  } else {
    detect <- 2
    v <- factor(x)
    x <- matrix(v, ncol=ncol(x))
    if (is.null(col)) col <- heat.colors(length(levels(v)))
    if (is.null(breaks)) breaks <- levels(v)
  }
  if (!detect) stop('non convertible data type')
  rowindex <- 1:nrow(x)
  colindex <- 1:ncol(x)
  cn <- colnames(x)
  if (is.null(cn)) cn <- as.character(colindex)
  rn <- rownames(x)
  if (is.null(rn)) rn <- as.character(rowindex)
  #
  args <- list(...)
  args$x    <- c(0.5, ncol(x)+0.5)
  args$y    <- c(0.5, nrow(x)+0.5)
  args$type <- 'n'
  if (is.null(args$main)) args$main <- main
  if (is.null(args$axes)) args$axes <- FALSE
  if (is.null(args$xlab)) args$xlab <- ''
  if (is.null(args$ylab)) args$ylab <- ''  
  if (is.null(args$xlim)) args$xlim <- c(0.5, ncol(x)+0.5+!is.null(key))
  if (is.null(args$ylim)) args$ylim <- c(0.5, nrow(x)+0.5)
  if (is.null(args$xaxs)) args$xaxs <- 'i'
  if (is.null(args$yaxs)) args$yaxs <- 'i'
  if (is.null(args$cex))  args$cex  <- 1
  do.call('plot', args)
  for (i in rowindex) {
    py  <- nrow(x)-i+1
    for (j in colindex) {
      xij <- x[i, j]
      if (is.na(xij)) {
        cij <- na.col 
      } else { 
        k   <- switch(detect, findInterval(xij, breaks, all.inside=TRUE), which(breaks==xij))
        cij <- col[k]
      }
      px  <- j
      polygon(c(px-0.5, px-0.5, px+0.5, px+0.5), c(py-0.5, py+0.5, py+0.5, py-0.5), col=cij)
      if (!is.na(digits)) {
        if (detect==1) text(px, py, sprintf('%+.*f', digits, xij), cex=args$cex, col=args$col)
        if (detect==2) text(px, py, sprintf('%s',  substr(xij, 1, digits)), cex=args$cex, col=args$col)
      }
    }
  }
  #box()
  axis(1, at=colindex, labels = cn, las=1)
  axis(2, at=rowindex, labels = rev(rn), las=1)
  if (!is.null(key)) {
    key$side   <- 4
    if(is.null(key$las)) key$las <- 1
    if (detect==1) {
      key$at     <- 1+(nrow(x)-1)*(0:(length(breaks)-1))/(length(breaks)-1)
      blocks     <- key$at
      if (is.na(digits)) digits <- as.integer(2-log10(diff(range(breaks))))
      key$labels <- sprintf('%+.*f', digits, breaks)
    }
    if (detect==2) {
      blocks     <- 1+(nrow(x)-1)*(0:length(breaks))/length(breaks)
      key$at     <- (blocks[-1]+blocks[-length(blocks)])/2
      key$labels <- if (is.na(digits)) breaks else substr(breaks, 1, digits)
    }
    do.call('axis', key)
    for (i in 1:length(col)) {
      polygon(c(ncol(x)+1, ncol(x)+1, ncol(x)+1.5, ncol(x)+1.5),
              c(blocks[i], blocks[i+1], blocks[i+1], blocks[i]),
              col=col[i])
    }
  }
}