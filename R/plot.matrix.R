#' plot.matrix
#'
#' Visualizes a matrix with a colored heatmap and optionally a color key. It distinguishes between numeric and non-numeric matrices. 
#' You may need to modify \code{mar} with the \code{\link[graphics]{par}} command from its default \code{c(5.1,4.1,4.1,2.1)}. 
#' For further see the vignette \code{vignette('plot.matrix')}
#'
#' @details 
#' A color key is drawn if either \code{key} (defaults to \code{list(cex=1)}) or \code{fmt.key} 
#' (defaults to \code{NULL}) is not \code{NULL}. 
#' 
#' If you want to plot the matrix entries you must set either \code{digits} or \code{fmt}. 
#' For a non-numeric matrix \code{digits} gives the length of the string printed, a negative value 
#' results in right-justified string. For a numeric matrix \code{digits} determines the number of 
#' decimal places,  a negative value uses a "exponential" decimal notation. You may set format 
#' strings \code{fmt} and \code{fmt.key} directly. Settings \code{digits} leads to the following 
#' format strings (\code{n} the absolute value of \code{digits}):
#' 
#' \tabular{ll}{
#' \code{x} numeric and \code{digits>0}:\tab \code{"\%+.nf"}\cr
#' \code{x} numeric and \code{digits<0}:\tab \code{"\%+.ne"}\cr
#' \code{x} non-numeric and \code{digits>0}:\tab \code{"\%+ns"}\cr
#' \code{x} non-numeric and \code{digits<0}:\tab \code{"\%-ns"}\cr
#' }
#' 
#' If no colors are given then the \code{\link[grDevices]{heat.colors}} will be used. Alternatively you may specify your own color function 
#' that delivers a vector with \code{n} colors if called by \code{col(n)}. The final colors and breaks used 
#' depend if \code{plot.matrix} gets a numeric or non-numeric matrix.
#' 
#' \strong{Numeric matrix:} In general it must hold \code{length(col)+1==length(breaks)}.
#'   \describe{
#'   \item{1. \code{breaks==NULL} and \code{col==NULL}}{The colors are taken from \code{heat.colors(10)} and the eleven breaks are calculated as an equidistant grid 
#'         between \code{min(x)} and \code{max(x)}.}
#'   \item{2. \code{breaks==NULL} and \code{col} is a color function}{Ten colors are taken from the color function and eleven breaks are calculated as an equidistant grid 
#'         between \code{min(x)} and \code{max(x)}.}
#'   \item{3. \code{breaks==NULL} and \code{col} is a vector of colors}{The \code{length(col)+1} breaks are calculated as an equidistant grid 
#'         between \code{min(x)} and \code{max(x)}.}  
#'   \item{4. \code{breaks} are given and \code{col==NULL}}{The colors are taken from \code{heat.colors(length(breaks)-1)}.}
#'   \item{5. \code{breaks} are given and \code{col} is a color function}{The \code{length(breaks)-1} colors are taken from the color function.}
#'   \item{6. \code{breaks} are given and \code{col} is a vector of colors}{If not \code{length(col)+1==length(breaks)} holds then 
#'   the \code{length(col)+1} breaks are calculated as an equidistant grid between \code{min(breaks)} and \code{max(breaks)}.}
#' }
#' 
#' \strong{Non-numeric matrix:} In general it must hold \code{length(col)==length(breaks)}. At first the number of unique elements in \code{x} is determined: \code{nu}.
#'   \describe{
#'   \item{1. \code{breaks==NULL} and \code{col==NULL}}{The colors are taken from \code{heat.colors(nu)} and the breaks are set to the unique elements of \code{x}.}
#'   \item{2. \code{breaks==NULL} and \code{col} is a color function}{The \code{nu} colors are taken from color function and the breaks are set to the unique elements of \code{x}.}
#'   \item{3. \code{breaks==NULL} and \code{col} is a vector of colors}{The \code{length(col)} breaks are calculated as an equidistant grid 
#'         between \code{min(x)} and \code{max(x)}.}  
#'   \item{4. \code{breaks} are given and \code{color==NULL}}{The colors are taken from \code{heat.colors(length(breaks))}.}
#'   \item{5. \code{breaks} are given and \code{color} is a color function}{The \code{length(breaks)} colors are taken from color function.}
#'   \item{6. \code{breaks} are given and \code{color} is a vector of colors}{If not \code{length(colors)==length(breaks)} holds then 
#'   either \code{breaks} or \code{color} is shorten to the shorter of both.}
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
#' @param key list of parameters used for \code{\link[graphics]{axis}}. If set to \code{NULL} then no information will be plotted. Instead of \code{key=list(side=4)} you may use \code{key=4} or \code{key="right"}. 
#' @param axis.key as \code{key}
#' @param na.col color for missing value (default: white)
#' @param fmt.cell format string for writring matrix entries, overwrites \code{digits}, defaults to \code{NULL}
#' @param fmt.key format string for writring key entries, overwrites \code{digits}, defaults to \code{fmt}
#' @param polygon.cell list of parameters used for \code{\link[graphics]{polygon}} for heatmap
#' @param polygon.key list of parameters used for \code{\link[graphics]{polygon}} for key
#' @param text.cell list of parameters used for \code{\link[graphics]{text}} for matrix entries
#' @param axis.col list of parameters used for \code{\link[graphics]{axis}} for axis of matrix columns. Instead of \code{axis.col=list(side=1)} you may use \code{axis.col=1} or \code{axis.col="bottom"}.
#' @param axis.row list of parameters used for \code{\link[graphics]{axis}} for axis of matrix rows. Instead of \code{axis.row=list(side=2)} you may use \code{axis.row=2} or \code{axis.col="left"}.
#' @param ... further parameter given to the \code{\link[graphics]{plot}} command
#' @return a plot
#' @importFrom grDevices heat.colors col2rgb
#' @importFrom graphics axis polygon text
#' @importFrom utils modifyList
#' @export
#' @aliases plot
#'
#' @examples
#' par(mar=c(5.1, 4.1, 4.1, 4.1))
#' # numeric matrix
#' x <- matrix(runif(50), nrow=10)
#' plot(x)
#' plot(x, key=NULL)
#' plot(x, key=list(cex.axis=0.5, tick=FALSE))
#' plot(x, digits=3)
#' plot(x, breaks=range(x), digits=3, cex=0.6)
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
#' plot(s, digits=1, col=heat.colors(5), breaks=letters[1:5])
#' plot(s, digits=1, col=heat.colors(5), breaks=c('a', 'c', 'e', 'g', 'i'))
#' # contingency table
#' tab <- table(round(rnorm(100)), round(rnorm(100)))
#' plot(unclass(tab))
#' # chisquare test residuals
#' cst <- chisq.test(apply(HairEyeColor, 1:2, sum))
#' col <- colorRampPalette(c("blue", "white", "red"))
#' plot(cst$residuals, col=col, breaks=c(-7.5, 7.5))
plot.matrix <- function(x, y=x, breaks=NULL, col=heat.colors, na.col="white", 
                        #
                        digits=NA, 
                        fmt.cell=NULL, 
                        fmt.key=NULL, 
                        #
                        polygon.cell=NULL,
                        polygon.key=NULL,
                        #
                        text.cell=NULL,
                        #
                        key=list(side=4, las=1),
                        axis.col=list(side=1),
                        axis.row=list(side=2),
                        axis.key=NULL,
                        #
                        ...) {
  # from https://stackoverflow.com/questions/13289009/check-if-character-string-is-a-valid-color-representation
  areColors <- function(x) {
    sapply(x, function(X) {
      tryCatch(is.matrix(col2rgb(X)), 
               error = function(e) FALSE)
    })
  }
  #
  createAxis <- function(defaults, globals, args) {
    if (is.null(defaults)) res <- list()
    if (!is.null(globals)) res <- modifyList(defaults, globals)
    if (is.numeric(args)) {
      if (args %in% 1:4) res$side <- args 
    }
    if (is.character(args)) {
      side <- pmatch(axis.col, c("bottom", "left", "top", "right"))
      if (!is.na(side)) res$side <- args 
    }
    if (is.list(args)) res <- modifyList(res, args)
    res
  }
  #
  main     <- paste(deparse(substitute(x)), collapse = "\n")
  ## determine colors and breaks: set of colors (=1) or color function (=2)
#  browser()
  color  <- assignColors(as.vector(x), breaks=breaks, col=col, na.col=na.col)
  col    <- attr(color, 'col')
  breaks <- attr(color, 'breaks')  
  ## determine matrix type: numerical (=1) or non-numerical (=2)
  matrixtype <- 0
  if (('numeric' %in% class(x[1,1])) | (('integer' %in% class(x[1,1])))) {
    matrixtype <- 1
  } else {
    matrixtype <- 2
  }
  if (!matrixtype) stop('non convertible data type')
  ## prepare formats
#  browser()
  if (is.null(fmt.cell)) {
    if (!is.na(digits)) {
      if (matrixtype==1) fmt.cell <- if (digits<0) sprintf("%%+.%.0fe", -digits) else sprintf("%%+.%.0ff", digits)
      if (matrixtype==2) fmt.cell <- if (digits<0) sprintf("%%+.%.0fs", -digits) else sprintf("%%+.%.0fs", digits)
    }
  }
  if (is.null(fmt.key)) {
    if (!is.na(digits)) {
      if (matrixtype==1) fmt.key <- if (digits<0) sprintf("%%+.%.0fe", -digits) else sprintf("%%+.%.0ff", digits)
      if (matrixtype==2) fmt.key <- if (digits<0) sprintf("%%+.%.0fs", -digits) else sprintf("%%+.%.0fs", digits)
    }
  }
  ## shall we plot the key?
  if (!(is.null(key) && is.null(fmt.key) && is.null(polygon.key) && is.null(axis.key))) {
    if (is.null(axis.key)) {
      axis.key <- if (is.null(key)) list() else key
      if (is.null(axis.key$side)) axis.key$side <- 4
    }
    if (is.null(fmt.key)) {
      if (matrixtype==1) {
        digits  <- as.integer(2-log10(diff(breaks)[1])) 
        fmt.key <- if (digits<0) sprintf("%%+.%.0fe", -digits) else sprintf("%%+.%.0ff", digits)
      }
      if (matrixtype==2) {   
        fmt.key <- "%s"
      }
    }
  } 
  ## prepare basic plot
  args <- ellipsis <- list(...)
  apar <- c('cex.axis', 'col.axis', 'col.ticks', 'font', 'font.axis', 'hadj', 'las', 
            'lwd.ticks', 'line' , 'outer', 'padj', 'tck', 'tcl', 'tick')
  ppar <- c('border', 'density', 'angle')
  tpar <- c('cex', 'font', 'vfont')
  #
  args[apar] <- NULL
  args[ppar] <- NULL
  args$x    <- c(0.5, ncol(x)+0.5)
  args$y    <- c(0.5, nrow(x)+0.5)
  args$type <- 'n'
  if (is.null(args$main)) args$main <- main
  if (is.null(args$axes)) args$axes <- FALSE
  dimn <- names(dimnames(x))
  if (is.null(args$xlab)) args$xlab <- if (is.null(dimn[2])) 'Column' else dimn[2]
  if (is.null(args$ylab)) args$ylab <- if (is.null(dimn[2])) 'Row' else dimn[1]
  d <- c(0,0,0,0,0)
  if (!is.null(axis.key)) d[axis.key$side] <- 1
  if (is.null(args$xlim)) args$xlim <- c(0.5-d[2], ncol(x)+0.5+d[4])
  if (is.null(args$ylim)) args$ylim <- c(0.5-d[1], nrow(x)+0.5+d[3])
  if (is.null(args$xaxs)) args$xaxs <- 'i'
  if (is.null(args$yaxs)) args$yaxs <- 'i'
  if (is.null(args$cex))  args$cex  <- 1
  do.call('plot', args) ### do.call
  ## draw matrix polygons
  # determine color
  colmat <- matrix(color, nrow=nrow(x), ncol=ncol(x))
  # build axes
  if (!is.null(axis.col)) axis.col <- createAxis(list(side=1), ellipsis[apar], axis.col)  
  if (!is.null(axis.row)) axis.row <- createAxis(list(side=2), ellipsis[apar], axis.row)
  if (!is.null(axis.key)) {
    axis.key <- createAxis(list(side=4), ellipsis[apar], axis.key)
  } else if(!is.null(key)) {
    axis.key <- createAxis(list(side=4), ellipsis[apar], key)
  }
  # check axes
  axes <- rep(0, 4)
  if (!is.null(axis.col)) { 
    if (axes[axis.col$side]) warning("Two axes at the same side")
    axes[axis.col$side] <- axes[axis.col$side]+1
  }
  if (!is.null(axis.row)) { 
    if (axes[axis.row$side]) warning("Two axes at the same side")
    axes[axis.row$side] <- axes[axis.row$side]+1
  }
  if (!is.null(axis.key)) { 
    if (axes[axis.key$side]) warning("Two axes at the same side")
    axes[axis.key$side] <- axes[axis.key$side]+1
  }
  # polygons
  pcell <- modifyList(list(), ellipsis[ppar])
  polygon.cell <- if (is.null(polygon.cell)) pcell else modifyList(pcell, polygon.cell)
  colindex <- 1:ncol(x)
  rowindex <- 1:nrow(x)
  # build text
  tcell <- modifyList(list(), ellipsis[tpar])  
  text.cell <- if (is.null(text.cell)) tcell else modifyList(tcell, text.cell)
  #
  if (is.null(polygon.cell)) polygon.cell <- list() 
  for (i in rowindex) {
    py             <- nrow(x)-i+1
    polygon.cell$y <- c(py-0.5, py+0.5, py+0.5, py-0.5)
    for (j in colindex) {
      px               <- j
      polygon.cell$x   <- c(px-0.5, px-0.5, px+0.5, px+0.5)
      polygon.cell$col <- colmat[i,j]
      do.call('polygon', polygon.cell) ### polygon
      if (!is.null(fmt.cell)) {
        if (matrixtype==1) sij <- x[i, j]
        if (matrixtype==2) sij <- if (is.na(digits)) x[i, j] else substr(x[i, j], 1, abs(digits))
        text.cell$x      <- px
        text.cell$y      <- py
        text.cell$labels <- sprintf(fmt.cell, sij)
        do.call('text', text.cell) ## text
      }
    }
  }
  # draw axes
  if (!is.null(axis.col)) {
    if (is.null(axis.col$labels)) {
      cn <- dimnames(x)[[2]]
      if (is.null(cn)) cn <- as.character(colindex)
      axis.col$labels <- cn
    }
    if (is.null(axis.col$at))   axis.col$at <- colindex
    do.call('axis', axis.col)
  }
  if (!is.null(axis.row)) {
    if (is.null(axis.row$labels)) {
      rn <- dimnames(x)[[1]]
      if (is.null(rn)) rn <- as.character(rowindex)
      axis.row$labels <- rev(rn)
    }
    if (is.null(axis.row$at)) axis.row$at   <- rowindex
    do.call('axis', axis.row)
  }
  ## draw if key necessary 
  if (!is.null(axis.key)) {
    if (matrixtype==1) {
      at <- (0:(length(breaks)-1))/(length(breaks)-1)
      if (axis.key$side %in% c(2,4)) axis.key$at <- 1+(nrow(x)-1)*at
      if (axis.key$side %in% c(1,3)) axis.key$at <- 1+(ncol(x)-1)*at     
      blocks     <- axis.key$at
    }
    if (matrixtype==2) {
      at <- (0:length(breaks))/length(breaks)
      if (axis.key$side %in% c(2,4)) blocks <- 1+(nrow(x)-1)*at
      if (axis.key$side %in% c(1,3)) blocks <- 1+(ncol(x)-1)*at    
      axis.key$at     <- (blocks[-1]+blocks[-length(blocks)])/2
    }
    axis.key$labels <- sprintf(fmt.key, breaks) 
    if (is.null(axis.key$las)) axis.key$las <- 1
    do.call('axis', axis.key) ### key axis
    pcell <- modifyList(list(), ellipsis[ppar])
    polygon.key <- if (is.null(polygon.key)) pcell else modifyList(pcell, polygon.key)
    for (i in 1:length(col)) {
      if (axis.key$side==1) {
        
      }
      if (axis.key$side==2) {
        polygon.key$x <- c(-0.5, -0.5, 0, 0)
        polygon.key$y <- c(blocks[i], blocks[i+1], blocks[i+1], blocks[i])        
      }
      if (axis.key$side==3) {
        polygon.key$x <- c(blocks[i], blocks[i+1], blocks[i+1], blocks[i])        
        polygon.key$y <- c(nrow(x)+1, nrow(x)+1, nrow(x)+1.5, nrow(x)+1.5)
      }
      if (axis.key$side==4) {
        polygon.key$x <- c(ncol(x)+1, ncol(x)+1, ncol(x)+1.5, ncol(x)+1.5)
        polygon.key$y <- c(blocks[i], blocks[i+1], blocks[i+1], blocks[i])
      }
      polygon.key$col <- col[i]
      do.call('polygon', polygon.key) ### polygon
    }
  }
}