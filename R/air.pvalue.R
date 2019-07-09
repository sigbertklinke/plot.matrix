#' New York Air Quality Measurements
#' 
#' p-values of pairwise correlation test of complete observations of the complete-cases of 
#' daily air quality measurements in New York, May to September 1973.
#' 
#' @docType data
#' 
#' @usage data(air.pvalue)
#' 
#' @format
#' A 4x4 matrix with p values of pairwise correlation tests (\code{\link[stats]{cor.test}}).
#'   \describe{
#'     \item{\code{Ozone}}{Ozone (ppb)}
#'     \item{\code{Solar.R}}{Solar R (lang)}
#'     \item{\code{Wind}}{Wind (mph)}
#'     \item{\code{Temp}}{Temperature (degrees F)}
#' }
#'    
#' @source  The data are derived from the \link[datasets:airquality]{New York Air Quality Measurements} data set.
#' @references  
#'   Chambers, J. M., Cleveland, W. S., Kleiner, B. and Tukey, P. A. (1983) Graphical Methods for Data Analysis. Belmont, CA: Wadsworth.
#' 
#' @examples
#' data(air.pvalue)
#' plot(as.pvalue(air.pvalue))
ind <- expand.grid(1:4, 1:4)
air.pvalue <- matrix(0, ncol=4, nrow=4)
air <- airquality[complete.cases(airquality),]
for (i in 1:nrow(ind)) {
  indi <- as.numeric(ind[i,])
  air.pvalue[indi[1],indi[2]] <- cor.test(air[,indi[1],], air[,indi[2]])$p.value
}
colnames(air.pvalue) <- rownames(air.pvalue) <- names(airquality)[1:4]
save(air.pvalue, file="data/air.pvalue.rda", version=2)
"air.pvalue"