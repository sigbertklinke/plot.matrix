#' plot.matrix
#' 
#' Visualizes a matrix with a colored heatmap and optionally a color key. It distinguishes between numeric and non-numeric matrices. 
#' You may need to modify \code{mar} with the [graphics::par()] command from its default \code{c(5.1,4.1,4.1,2.1)}. 
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
#' If no colors are given then the [grDevices::heat.colors()] will be used. Alternatively you may specify your own color function 
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
#' If the difference between polygon color and the text color is smaller \code{max.col} then as text color is 
#' either \code{white} or \code{black} (depending which one is further away from the polygon color). 
#' The distance is computed as \eqn{\Delta C/3} as in \url{https://en.wikipedia.org/wiki/Color_difference#Euclidean} given.
#'
#' @note The use of \code{fmt} or \code{fmt.key} have the same restrictions as the use of \code{fmt} in [base::sprintf()]: 
#' 
#' \emph{The format string is passed down the OS's sprintf function, and incorrect formats can cause the latter to crash the R process. 
#' R does perform sanity checks on the format, but not all possible user errors on all platforms have been tested, and some might 
#' be terminal.}
#' 
#' @param x matrix
#' @param y unused
#' @param digits number of digits for numeric data or length of string for non-numeric data 
#' @param col a vector of colors or a function, e.g. grDevices::heat.colors()] with one parameter \code{n} 
#' @param breaks breaks for numeric values or values for \code{col}
#' @param key list of parameters used for [graphics::axis(). If set to \code{NULL} then no information will be plotted. Instead of \code{key=list(side=4)} you may use \code{key=4} or \code{key="right"}. 
#' @param spacing.key spacing between plot and legend, key width, spacing between key and axis  (default: \code{c(1,0.5,0)})
#' @param axis.key as \code{key}
#' @param na.col color for missing value (default: white)
#' @param na.cell to draw cells with missing values (default: \code{TRUE})
#' @param na.print print NA (or any given characters) when values are missing. If \code{FALSE}, nothing is printed. If \code{na.cell} is \code{FALSE}, this will have no effect.
#' @param fmt.cell format string for writing matrix entries, overwrites \code{digits}, defaults to \code{NULL}
#' @param fmt.key format string for writing key entries, overwrites \code{digits}, defaults to \code{fmt}
#' @param polygon.cell list of parameters used for [graphics::polygon()] for heatmap
#' @param polygon.key list of parameters used for [graphics::polygon()] for key
#' @param text.cell list of parameters used for [graphics::text()] for matrix entries
#' @param axis.col list of parameters used for [graphics::axis()] for axis of matrix columns. Instead of \code{axis.col=list(side=1)} you may use \code{axis.col=1} or \code{axis.col="bottom"}.
#' @param axis.row list of parameters used for [graphics::axis()] for axis of matrix rows. Instead of \code{axis.row=list(side=2)} you may use \code{axis.row=2} or \code{axis.col="left"}.
#' @param max.col numeric: if the distance between the text color and the cell color is smaller then \code{max.col} then either \code{white} or \code{black} will be used as text color, defaults to \code{70}
#' @param ... further parameter given to the [graphics::plot()] command
#' @return invisibly a list with elements 
#' \describe{
#' \item{\code{cell.polygon[[i,j]]}}{the \code{polygon} parameters used to draw the elements of the matrix}
#' \item{\code{cell.text[[i,j]]}}{the \code{text} parameters used to draw the elements of the matrix}
#' \item{\code{plot}}{the \code{plot} parameters used to draw the basic plot}
#' \item{\code{axis.col}}{the \code{axis} parameters used to draw column axis}
#' \item{\code{axis.row}}{the \code{axis} parameters used to draw row axis}
#' \item{\code{key.polygon[[i]]}}{the \code{polygon} parameters used to draw the elements of the key}
#' \item{\code{key.axis}}{the \code{axis} parameters used to draw key axis}
#' }
#' A \code{NULL} means the elements has not been drawn.
#' @importFrom grDevices heat.colors col2rgb
#' @importFrom graphics axis polygon text
#' @importFrom utils modifyList
#' @md
#' @export 
#' @method plot matrix
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
#' plot(cst$residuals, col=col, breaks=c(-7.5,7.5))
#' # triangular matrix
#' x[upper.tri(x)] <- NA
#' plot(x, digit=2)
#' plot(x, na.print=FALSE)
#' plot(x, na.cell=FALSE)
#' # use the standard plot instead of plot.matrix
#' x <- matrix(runif(50), nrow=2)
#' plot(as.data.frame(x))
#' plot.default(x)
#' \dontrun{
#' # unload the package permanently with devtools
#' library("devtools")
#' unload('plot.matrix')
#' }
plot.matrix <- function(x, y=NULL, breaks=NULL, col=heat.colors, 
                        #
                        na.col="white", 
                        na.cell=TRUE,
                        na.print=TRUE,
                        #
                        digits=NA, 
                        fmt.cell=NULL, 
                        fmt.key=NULL, 
                        #
                        spacing.key = c(1, 0.5, 0),
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
                        max.col=70,
                        #
                        ...) {
  # from https://stackoverflow.com/questions/13289009/check-if-character-string-is-a-valid-color-representation
  areColors <- function(x) {
    sapply(x, function(X) {
      tryCatch(is.matrix(col2rgb(X)), 
               error = function(e) FALSE)
    })
  }
  # from https://en.wikipedia.org/wiki/Color_difference#Euclidean
  colorDist <- function(c1, c2) {
    rgb1 <- col2rgb(c1)
    rgb2 <- col2rgb(c2)
    rq <- (rgb1[1]+rgb2[1])/2
    dr <- rgb1[1]-rgb2[1]
    dg <- rgb1[2]-rgb2[2]
    db <- rgb1[3]-rgb2[3]
    sqrt((2+rq/256)*dr*dr+4*dg*dg+(2+(255-rq)/256)*db*db)/3
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
  ret      <- list()
  main     <- paste(deparse(substitute(x)), collapse = "\n")
  ### determine color type: set of colors (=1) or color function (=2)
  #coltype <- 0
  #if (is.null(col)) { 
  #  col  <- heat.colors
  #  coltype <- 2
  #} else if (is.function(col)) {
  #  coltype <- 2
  #} else if (all(areColors(col))) {
  #  coltype <- 1
  #}
  #if (!coltype) stop('non convertible color type')
  ## determine colors and breaks: set of colors (=1) or color function (=2)
  #browser()
  color  <- assignColors(as.vector(x), breaks=breaks, col=col, na.col=na.col)
  col    <- attr(color, 'col')
  breaks <- attr(color, 'breaks')  
  ## determine matrix type: numerical (=1) or non-numerical (=2)
  matrixtype <- 0
  if (('numeric' %in% class(x[1,1])) | (('integer' %in% class(x[1,1])))) {
    matrixtype  <- 1
#    #
#    if (is.null(breaks)) {
#      if (coltype==1) {
#        breaks <- seq(from=min(x, na.rm=TRUE), to=max(x, na.rm=TRUE), length.out=length(col)+1)
#      }
#      if (coltype==2) {
#        col     <- col(10)
#        coltype <- 1
#        breaks <- seq(from=min(x, na.rm=TRUE), to=max(x, na.rm=TRUE), length.out=length(col)+1)
#      }
#    } else {
#      if (coltype==1) {
#        if (length(col)+1!=length(breaks)) breaks <- seq(from=min(breaks, na.rm=TRUE), to=max(breaks, na.rm=TRUE), length.out=length(col)+1)
#      }
#      if (coltype==2) {
#        if (length(breaks)<3) {
#          col     <- col(10)
#          coltype <- 1
#          breaks <- seq(from=min(breaks, na.rm=TRUE), to=max(breaks, na.rm=TRUE), length.out=length(col)+1)          
#        } else {
#          col     <- col(length(breaks)-1)
#          coltype <- 1
#        }
#      }
#    }
#    if (length(col)+1!=length(breaks)) stop("colors and breaks do not match")
  } else {
    matrixtype <- 2
#    v <- factor(x)
#    x <- matrix(v, ncol=ncol(x))
#    if (coltype==1) {
#      if (is.null(breaks)) breaks <- levels(v)
#      if (length(col)<length(breaks)) breaks <- breaks[1:length(col)]
#      if (length(breaks)<length(col)) col <- col[1:length(breaks)]
#    }
#    if (coltype==2) {
##      if (is.null(breaks)) breaks <- levels(v)
#      col <- col(length(breaks))
#    }
#    if (length(col)!=length(breaks)) stop("colors and breaks do not match")
  }
  if (!matrixtype) stop('non convertible data type')
  ## prepare formats
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
    } else {
      if (matrixtype==1) {
        digits  <- as.integer(2-log10(diff(breaks)[1])) 
        fmt.key <- if (digits<0) sprintf("%%+.%.0fe", -digits) else sprintf("%%+.%.0ff", digits)
      }
      if (matrixtype==2) {   
        fmt.key <- "%s"
      }
    }
  }
#  ## shall we plot the key?
#  if (!(is.null(key) && is.null(fmt.key) && is.null(polygon.key) && is.null(axis.key))) {
#    if (is.null(axis.key)) {
#      axis.key <- if (is.null(key)) list() else key
#      if (is.null(axis.key$side)) axis.key$side <- 4
#    }
  ## shall we plot the key?
  if (!(is.null(key))) {
    if (!(is.null(fmt.key) && is.null(polygon.key) && is.null(axis.key))) {
      if (is.null(axis.key)) {
        axis.key <- if (is.null(key)) list() else key
        if (is.null(axis.key$side)) axis.key$side <- 4
      }
    } 
    if (length(spacing.key)==1) spacing.key <- c(spacing.key, 0.5, 0)
    if (length(spacing.key)==2) spacing.key <- c(spacing.key, 0)
    if (length(spacing.key)>3) warning("Parameter 'spacing.key' is too long")
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
  if (!is.null(axis.key)) d[axis.key$side] <- sum(spacing.key)
  if (is.null(args$xlim)) args$xlim <- c(0.5-d[2], ncol(x)+0.5+d[4])
  if (is.null(args$ylim)) args$ylim <- c(0.5-d[1], nrow(x)+0.5+d[3])
  if (is.null(args$xaxs)) args$xaxs <- 'i'
  if (is.null(args$yaxs)) args$yaxs <- 'i'
  if (is.null(args$cex))  args$cex  <- 1
  ret$plot <- args
  do.call('plot', args, quote=TRUE) ### do.call
  ## draw matrix polygons
  # determine color
  colmat <- matrix(color, nrow=nrow(x), ncol=ncol(x))
#  color <- c(na.col, col)
#  if (matrixtype==1) {
#    index <- 1+findInterval(x, breaks)
#    ones  <- (index==length(breaks)+1) | is.na(index)
#    index[ones] <- 1
#    max   <- which(x==max(breaks))
#    index[max] <- length(color)
#  }
#  if (matrixtype==2) index <- 1+match(x, breaks, nomatch=0)
#  color <- matrix(color[index], ncol=ncol(x))
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
  if (!na.cell) na.print <- FALSE ### prevent printing cell content without cell polygon
  if (is.logical(na.print)) {
    if (!na.print) na.print <- ""
  } else {
    if (length(na.print) > 1) warning("Only the first value of \"na.print\" was used.")
    na.print <- as.character(na.print)[1]
  }
  pcell <- modifyList(list(), ellipsis[ppar])
  polygon.cell <- if (is.null(polygon.cell)) pcell else modifyList(pcell, polygon.cell)
  colindex <- 1:ncol(x)
  rowindex <- 1:nrow(x)
  # build text
  tcell <- modifyList(list(), ellipsis[tpar])  
  text.cell <- if (is.null(text.cell)) tcell else modifyList(tcell, text.cell)
  #
  ret$cell.polygon      <- vector("list", length(x))
  dim(ret$cell.polygon) <- dim(x) 
  ret$cell.text         <- vector("list", length(x))
  dim(ret$cell.text)    <- dim(x) 
  #
  if (is.null(polygon.cell)) polygon.cell <- list() 
  for (i in rowindex) {
    py             <- nrow(x)-i+1
    polygon.cell$y <- c(py-0.5, py+0.5, py+0.5, py-0.5)
    for (j in colindex) {
      px               <- j
      polygon.cell$x   <- c(px-0.5, px-0.5, px+0.5, px+0.5)
#      polygon.cell$col <- color[i,j]
      polygon.cell$col <- colmat[i,j] 
      if (!(!na.cell && (is.na(x[i,j]) || x[i,j] == "NA"))) {
        do.call('polygon', polygon.cell) ### polygon
      }
      ret$cell.polygon[[i,j]] <- polygon.cell # save coordinates
      if (!is.null(fmt.cell)) {
        if (is.na(x[i,j]) || x[i,j] == "NA") {
          text.cell$labels <- "NA"
        } else {
          if (matrixtype==1) sij <- x[i, j]
          if (matrixtype==2) sij <- if (is.na(digits)) x[i, j] else substr(x[i, j], 1, abs(digits))
          text.cell$labels <- sprintf(fmt.cell, sij)
        }
        text.cell$x      <- px
        text.cell$y      <- py
        #
        if (is.character(na.print)) text.cell$labels[text.cell$labels == "NA"] <- na.print
        # if background color and text color are too similar
        tcc <- if (is.null(text.cell$col)) 'black' else text.cell$col
        rcl <- NULL
        if (colorDist(tcc, polygon.cell$col)<max.col) { 
          if (colorDist(tcc,'white')>colorDist(tcc,'black')) rcl <- 'white' else rcl <- 'black'
        }
        if (!is.null(rcl)) { tcc <- text.cell$col; text.cell$col <- rcl}
        #
        do.call('text', text.cell) ## text
        ret$cell.text[[i,j]] <- text.cell
        #
        if (!is.null(rcl)) { text.cell$col <- tcc}
        #
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
    ret$axis.col <- axis.col
  }
  if (!is.null(axis.row)) {
    if (is.null(axis.row$labels)) {
      rn <- dimnames(x)[[1]]
      if (is.null(rn)) rn <- as.character(rowindex)
      axis.row$labels <- rev(rn)
    }
    if (is.null(axis.row$at))   axis.row$at   <- rowindex
    do.call('axis', axis.row)
    ret$axis.row <- axis.row
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
    ret$key.axis <- axis.key
    pcell <- modifyList(list(), ellipsis[ppar])
    polygon.key <- if (is.null(polygon.key)) pcell else modifyList(pcell, polygon.key)
    ret$key.polygon <- vector("list", length(col))
    for (i in 1:length(col)) {
      if (axis.key$side==1) {
        polygon.key$x <- c(blocks[i], blocks[i+1], blocks[i+1], blocks[i])      
        polygon.key$y <- c(-spacing.key[2]-spacing.key[1]+0.5, -spacing.key[2]-spacing.key[1]+0.5, -spacing.key[1]+1, -spacing.key[1]+1)
      }
      if (axis.key$side==2) {
        polygon.key$x <- c(-spacing.key[2]-spacing.key[1]+0.5, -spacing.key[2]-spacing.key[1]+0.5, -spacing.key[1]+1, -spacing.key[1]+1)
        polygon.key$y <- c(blocks[i], blocks[i+1], blocks[i+1], blocks[i])        
      }
      if (axis.key$side==3) {
        polygon.key$x <- c(blocks[i], blocks[i+1], blocks[i+1], blocks[i])        
        polygon.key$y <- c(nrow(x)+spacing.key[1], nrow(x)+spacing.key[1], nrow(x)+spacing.key[1]+spacing.key[2], nrow(x)+spacing.key[1]+spacing.key[2])
      }
      if (axis.key$side==4) {
        polygon.key$x <- c(ncol(x)+spacing.key[1], ncol(x)+spacing.key[1], ncol(x)+spacing.key[1]+spacing.key[2], ncol(x)+spacing.key[1]+spacing.key[2])
        polygon.key$y <- c(blocks[i], blocks[i+1], blocks[i+1], blocks[i])
      }
      polygon.key$col <- col[i]
      do.call('polygon', polygon.key) ### polygon
      ret$key.polygon[[i]] <- polygon.key
    }
  }
  invisible(ret)
}
