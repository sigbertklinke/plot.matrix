if (suppressWarnings(require("vcd", quietly = TRUE))) {
  Titanic.cramer <- matrix(1, ncol=4, nrow=4)
  colnames(Titanic.cramer) <- rownames(Titanic.cramer) <- names(dimnames(Titanic))
  for (i in 1:3) {
    for (j in (i+1):4) {
      Titanic.cramer[i,j] <- Titanic.cramer[j,i] <- assocstats(apply(Titanic, c(i,j), sum))$cramer
    }
  }
  save(Titanic.cramer, file="data/Titanic.cramer.rda", version=2)
}
