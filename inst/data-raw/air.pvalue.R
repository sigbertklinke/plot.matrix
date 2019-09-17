ind <- expand.grid(1:4, 1:4)
air.pvalue <- matrix(0, ncol=4, nrow=4)
air <- airquality[complete.cases(airquality),]
for (i in 1:nrow(ind)) {
  indi <- as.numeric(ind[i,])
  air.pvalue[indi[1],indi[2]] <- cor.test(air[,indi[1],], air[,indi[2]])$p.value
}
colnames(air.pvalue) <- rownames(air.pvalue) <- names(airquality)[1:4]
save(air.pvalue, file="data/air.pvalue.rda", version=2)