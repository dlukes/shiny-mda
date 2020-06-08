library(ggplot2)
library(dplyr)

genreDiff <- function(data, ids1, ids2) {
  data <- select(data, -X, -MODE, -DIVISION, -DIVISION_ORIG, -SUPERCLASS, -CLASS)
  ncols <- ncol(data)
  results = data.frame(Factor=names(data),
                       Diff=rep(0, ncols),
                       relDiff=rep(0, ncols))
  data.melt = data.frame()
  for (i in 1:ncols) {
    myRange = quantile(data[,i], probs=0.9) - quantile(data[,i], probs=0.1)
    median1 = median(data[ids1, i], na.rm=TRUE)
    median2 = median(data[ids2, i], na.rm=TRUE)
    results[i, 2] = abs(median1 - median2)
    results[i, 3] = abs(median1 - median2) / myRange
    tmp1 = data.frame(Value=data[ids1, i], Factor=names(data)[i], Subcorpus="1")
    tmp2 = data.frame(Value=data[ids2, i], Factor=names(data)[i], Subcorpus="2")
    data.melt = rbind(data.melt, tmp1, tmp2)
  }
  ggplot(data.melt, aes(y=Value, x=Factor, fill=Subcorpus)) + geom_boxplot(outlier.alpha=0.5)
}
