#' plot.matrix
#'
#' Visualizes a matrix with a heatmap and distinguishes between numeric and non-numeric matrices. 
#' You may need to modify \code{mar} with the \code{\link[graphics]{par}} command from its default \code{c(5.1,4.1,4.1,2.1)}. 
#'
#' \code{plot.matrix} behaves differently if a numeric or non-numeric matrix is given.
#' 
#' \describe{
#'   \item{Numeric matrix}{
#'   \describe{
#'     \item{col}{Is a vector of colors or a function which generates with \code{col(n)} a color vector of length \code{n}. 
#'       The default length is \code{n=10}. If \code{col} is not given then \code{heat.color(10)} is used.
#'       }
#'     \item{breaks}{If not given then \code{breaks} is recalculated as an equidistant grid between \code{min(x)} and \code{max(x)}. 
#'       Otherwise the length of \code{breaks} should be one larger than the length of \code{col}. 
#'       If lengths does not match then then \code{breaks} is recalculated as an equidistant grid between \code{min(breaks)} 
#'       and \code{max(breaks)}.
#'       }
#'     \item{digits}{Determines the output into cells and the key plot. If \code{digits} is not given then in the cell nothing will be written.
#'       If \code{digits<0} then the format string \code{%+.*e} otherwise \code{%+.*f} is used (* is replaced by \code{abs(digits)}). 
#'       For details see \link[base]{sprintf}.
#'       }
#'     } 
#'   }
#'   \item{Non-numeric matrix}{
#'   \describe{
#'     \item{col}{Is a vector of colors or a function which generates with \code{col(n)} a color vector of length \code{n}. 
#'       The default length is the number of unique values of \code{x}. If \code{col} is not given then 
#'       \code{heat.color} with default length is used.}
#'     \item{breaks}Iit should hold \code{length(breaks)==length(col)}. If not given then \code{breaks} is computed as the unique values of \code{x}}
#'     \item{digits}{Determines the output into cells and the key plot. If \code{digits} is not given then in the cell nothing will be written.
#'       If \code{digits<0} then the format string \code{%+*s} otherwise \code{%+-*s} is used (* is replaced by \code{abs(digits)}). 
#'       For details see \link[base]{sprintf}. 
#'       }
#'     }
#'   }
#' }
#' 
#' @note The use of \code{fmt} or \code{fmt.key} have the same restrictions as the use of \code{fmt} in \code{\link[base]{sprintf}}: 
#' 
#' \emph{The format string is passed down the OS's sprintf function, and incorrect formats can cause the latter to crash the R process. 
#' R does perform sanity checks on the format, but not all possible user errors on all platforms have been tested, and some might 
#' be terminal.}
#' 
#' @param x matrix
#' @param y unused
#' @param digits number of digits for numeric data or length of string for non-numeric data 
#' @param col a vector of colors or a function, e.g. \code{\link[grDevices]{heat.colors}} with one parameter \code{n} 
#' @param breaks breaks for numeric values or values for \code{col}
#' @param key list of parameters used for \code{\link[graphics]{axis}}. If set to \code{NULL} then no information will be plotted. 
#' @param na.col color for missing value (default: white)
#' @param fmt format string for writring matrix entries, overwrites \code{digits}
#' @param fmt.key format string for writring key entries, overwrites \code{digits}. Default ist \code{fmt}
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
#' plot(m, col=c("red", "blue"))
#' plot(m, key=NULL, digits=1)
#' # character matrix
#' s <- matrix(sample(letters[1:10], 50, replace=TRUE), nrow=10)
#' plot(s)
#' plot(s, col=topo.colors)
#' plot(s, digits=10)
#' plot(s, digits=10, col=heat.colors(5), breaks=letters[1:5])
#' # contingency table
#' tab <- table(round(rnorm(100)), round(rnorm(100)))
#' plot(unclass(tab))
plot.matrix <- function(x, y=NULL, digits=NA, col=NULL, breaks=NULL, key=list(cex.axis=1), na.col="white", fmt=NULL, fmt.key=fmt, ...) {
  main     <- paste(deparse(substitute(x)), collapse = "\n")
  # type
  detect <- 0
  if (('numeric' %in% class(x[1,1])) | (('integer' %in% class(x[1,1])))) {
    detect <- 1
    if (is.null(col))     col <- heat.colors(10)
    if (is.function(col)) col <- col(10)
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
    if (is.null(col))     col <- heat.colors(length(levels(v)))
    if (is.function(col)) col <- col(10)   
    if (is.null(breaks)) breaks <- levels(v)
  }
  # formatting
  if (is.null(fmt)) {
    if (!is.na(digits)) {
      if (detect==1) {
        fmt <- if (digits<0) sprintf("%%+.%.0fe", -digits) else sprintf("%%+.%.0ff", digits)
      }
      if (detect==2) {     
        fmt <- if (digits<0) sprintf("%%+.%.0fs", -digits) else sprintf("%%+.%.0fs", digits)
      }
    }
  }
  browser()
  if (is.null(fmt.key)) {
    if (detect==1) {
      if (is.na(digits)) digits <- as.integer(2-log10(diff(breaks)[1])) 
      fmt.key <- if (digits<0) sprintf("%%+.%.0fe", -digits) else sprintf("%%+.%.0ff", digits)
    }
    if (detect==2) {   
      if (is.na(digits)) {
        fmt.key <- "%s"
      } else {
        fmt.key <- if (digits<0) sprintf("%%+.%.0fs", -digits) else sprintf("%%+.%.0fs", digits)
      }
    }
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
      if (!is.null(fmt)) {
        if (detect==1) sij <- xij
        if (detect==2) sij <- if (is.na(digits)) xij else substr(xij, 1, abs(digits))
        text(px, py, sprintf( fmt, sij), cex=args$cex, col=args$col)
      }
    }
  }
  #box()
  axis(1, at=colindex, labels = cn, las=1)
  axis(2, at=rowindex, labels = rev(rn), las=1)
  if (!is.null(fmt.key)) {
    key$side   <- 4
    if(is.null(key$las)) key$las <- 1
    if (detect==1) {
      key$at     <- 1+(nrow(x)-1)*(0:(length(breaks)-1))/(length(breaks)-1)
      blocks     <- key$at
      key$labels <- sprintf(fmt.key, breaks) 
    }
    if (detect==2) {
      blocks     <- 1+(nrow(x)-1)*(0:length(breaks))/length(breaks)
      key$at     <- (blocks[-1]+blocks[-length(blocks)])/2
    }
    key$labels <- sprintf(fmt.key, breaks) 
    do.call('axis', key)
    for (i in 1:length(col)) {
      polygon(c(ncol(x)+1, ncol(x)+1, ncol(x)+1.5, ncol(x)+1.5),
              c(blocks[i], blocks[i+1], blocks[i+1], blocks[i]),
              col=col[i])
    }
  }
}