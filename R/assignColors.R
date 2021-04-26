#' assignColors
#'
#' Assign to each value in \code{x} a color according to the choices of \code{breaks} and \code{col}.
#'
#' @details Depending if \code{x} is a numeric or non-numeric vector colors 
#' are assigned to each value. 
#' 
#' In case of a numeric vector \code{breaks} can be 
#' \itemize{
#' \item a number, giving the number of intervals covering the range of \code{x},
#' \item a vector of two numbers, given the range to cover with 10 intervals, or
#' \item a vector with more than two numbers, specify the interval borders
#' }
#' In case of a non-numeric vector \code{breaks} must contain all values which are
#' will get a color.
#' If \code{breaks} is not given then a sensible default is choosen: 
#' in case of a numeric vector derived from \code{\link[base]{pretty}} and 
#' otherwise all unique values/levels are used.
#' 
#' \code{col} can be either be a vector of colors or a function which generates
#' via \code{col(n)} a set of \code{n} colors. The default is to use 
#' \code{\link[grDevices]{heat.colors}}.
#' 
#' Possible color functions in R packages can be found by \code{vignette('plot.matrix')}.
#' 
#' @param x numeric or non-numeric vector
#' @param breaks vector with breaks
#' @param col vector with colors or color function
#' @param na.col color for NA or out-of-range values
#'
#' @return vector of color with the same length as \code{x} with the attributes
#' \code{breaks} the breaks used, \code{col} the color coding and \code{na.col} 
#' the color for \code{NA} and out-of-range entries
#' @export 
#'
#' @examples
#' ## numeric vector
#' x <- runif(10)
#' assignColors(x)
#' # set breaks
#' assignColors(x, breaks=15)
#' assignColors(x, breaks=c(0,1))
#' # set colors
#' assignColors(x, col=c("red", "green", "blue"))
#' assignColors(x, col=topo.colors)
#' # NA and out-of-range
#' x[5] <- NA
#' assignColors(x, breaks=seq(0.5, 1, by=0.1), na.col="red")
#' ## logical vector
#' l <- sample(c(NA, TRUE, FALSE), size=10, replace=TRUE)
#' assignColors(l)
#' assignColors(l, breaks=c("FALSE", "TRUE"), col=c("red", "blue"))
#' ## character vector
#' t <- sample(letters, size=10, replace=TRUE)
#' assignColors(t)
#' assignColors(t, col=rainbow(5))
assignColors <- function(x, breaks=NULL, col=heat.colors, na.col="white") {
  areColors <- function(x) {
    sapply(x, function(X) {
      tryCatch(is.matrix(col2rgb(X)), 
               error = function(e) FALSE)
    })
  }
  #
  if (is.factor(x))  {
    if (is.null(breaks)) breaks <- levels(x)
    x <- as.character(x)
  }
  if (is.logical(x)) {
    x <- ifelse(x, 'TRUE', 'FALSE')
    if (is.null(breaks)) breaks <- c('FALSE', 'TRUE')
  }
  color <- NULL
  if (is.character(x)) {
    if (is.null(breaks)) breaks <- sort(unique(x))
    colpal <- if (is.function(col)) col(length(breaks)) else col
    color  <- rep(na.col, length(x))
    for (i in seq(breaks)) {
      index <- which(x==breaks[i]) 
      if (i>length(colpal)) break
      if (length(index)) color[index] <- colpal[i]
    }
    if (length(colpal)<length(breaks)) breaks <- breaks[seq(colpal)]
  }
  if (is.numeric(x)) {
    if (is.null(breaks)) {
      breaks <- pretty(x)
      if (!is.function(col)) breaks <- seq(from=min(breaks, na.rm=TRUE), to=max(breaks, na.rm=TRUE), length.out=1+length(col))
    } else {
      if (length(breaks)==2) {
        breaks <- seq(from=min(breaks, na.rm=TRUE), to=max(breaks, na.rm=TRUE), length.out=11)
      }  
      if (length(breaks)==1) {
        px     <- pretty(x)
        breaks <- seq(from=min(px), to=max(px), length.out=breaks+1)
      }
    }
    N      <- length(breaks)
    colpal <- if (is.function(col)) col(N-1) else col      
    index  <- findInterval(x, breaks, all.inside=TRUE) 
    color  <- colpal[index]
    nacol  <- is.na(x) | (x<min(breaks)) | (x>max(breaks))
    if (any(nacol)) color[nacol] <- na.col
  }
  if (any(!areColors(colpal))) stop('non convertible color type')
  if (is.null(color)) stop(sprintf('unknown vector class: %s', paste0(class(x), collapse=', ')))
  attr(color, 'breaks') <- breaks
  attr(color, 'col')    <- colpal
  attr(color, 'na.col') <- na.col
  color
}