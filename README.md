# plot.matrix
Visualizes a matrix object plainly as heatmap. It provides a single S3 function `plot` for matrices and a function `assignColors` which assigns a specific color to each value of a vector.

# Installation  

## From CRAN

```R
install.packages("plot.matrix")
```

## From github

Note that from github you install the current development version.

```R
library("devtools")
install_github("sigbertklinke/plot.matrix")
```

# Examples

## `plot.matrix`

```R
# you may need to adjust the margin sizes
library("plot.matrix")
par(mar=c(5.1, 4.1, 4.1, 4.1)) # default c(5.1, 4.1, 4.1, 2.1)
```

```R
# numeric matrix
x <- matrix(runif(50), nrow=10)
plot(x)
plot(x, key=NULL)
plot(x, key=list(cex.axis=0.5, tick=FALSE))
plot(x, digits=3)
plot(x, breaks=c(0,1), digits=3, cex=0.6)
```

```R
# logical matrix
m <- matrix(runif(50)<0.5, nrow=10)
plot(m)
plot(m, key=NULL, digits=1)
```

```R
# character matrix
s <- matrix(sample(letters[1:10], 50, replace=TRUE), nrow=10)
plot(s)
plot(s, digits=10)
plot(s, digits=10, col=heat.colors(5), breaks=letters[1:5])
```

## `plot.loadings`

```R
# factor analysis
library("psych")
data <- na.omit(bfi[,1:25]))
#
fa1 <- factanal(data, 5)
plot(loadings(fa1))
#
fa2 <- fa(data, 5) # psych::fa
plot(loadings(fa2))
```

```R
# principal component analysis
library("psych")
data <- na.omit(bfi[,1:25]))
# 
pa <- princomp(data)
plot(loadings(pa), digits=NA) # no numbers
#
pa <- prcomp(data)
ld <- structure(pa$rotation, class="loadings")
plot(ld, digits=NA)
```

## `assignColors`

```R
# numeric vector
assignColors(runif(50))
# logical vector
assignColors(runif(50)<0.5) 
# character vector
assignColors(sample(letters[1:10], 50, replace=TRUE))
```

# History
  * 2019-07-03 Added bfi data set
  * 2019-05-10 plot.loadings, error in y-axis labelling
  * 2018-12-20 Vignette, assignColors
  * 2018-12-01 Added examples for tables, extended formatting possibilities
  * 2018-11-27 Version 1.0
