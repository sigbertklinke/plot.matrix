# plot.matrix
Visualizes a matrix object plainly as heatmap. It provides a single S3 function plot for matrices.

# Installation  

## From github

```R
library("devtools")
install_github("sigbertklinke/plot.matrix")
```

# Examples

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
