# Usage example:
# source("load_data.R")
# model1 = "2017-09-18_4f.RData"
# ref = loadData(file.path("results", model1))
# model2 = "2017-05-18.RData"
# cmp = loadData(file.path("results", model2))
# ModelCmp(ref$ldf, ref$fdf, cmp$ldf, cmp$fdf, model1, model2)

library(ggplot2)
library(tidyr)
library(dplyr)
library(scales)

ModelCmp <- function(ldf1, fdf1, ldf2, fdf2, modelName1, modelName2) {
  fdf1 = select(fdf1, -MODE, -DIVISION, -SUPERCLASS, -CLASS, -X)
  fdf2 = select(fdf2, -MODE, -DIVISION, -SUPERCLASS, -CLASS, -X)
  cormat = data.frame(Type = vector(mode="character", length=0),
                      Model1 = vector(mode="character", length=0),
                      Model2 = vector(mode="character", length=0),
                      r = vector(mode="logical", length=0))
  cormat$Type = as.character(cormat$Type)
  cormat$Model1 = as.character(cormat$Model1)
  cormat$Model2 = as.character(cormat$Model2)
  for(f1 in levels(ldf1$Factor)) {
    val1 = filter(ldf1, Factor == f1) %>% select(-Factor)
    val1$Feature = as.character(val1$Feature)
    for (f2 in levels(ldf2$Factor)) {
      val2 = filter(ldf2, Factor == f2) %>% select(-Factor)
      val2$Feature = as.character(val2$Feature)
      values = inner_join(val1, val2, by="Feature")
      rs = cor(values[,2], values[,3], method="s")
      tmp = data.frame(Type = "Features", Model1 = f1, Model2 = f2, r = rs)
      tmp$Type = as.character(tmp$Type)
      tmp$Model1 = as.character(tmp$Model1)
      tmp$Model2 = as.character(tmp$Model2)
      cormat = bind_rows(cormat, tmp)
    }
  }
  for(f1 in colnames(fdf1)) {
    val1 = data.frame(Chunk = rownames(fdf1), load = fdf1[,f1])
    val1$Chunk = as.character(val1$Chunk)
    for (f2 in colnames(fdf2)) {
      val2 = data.frame(Chunk = rownames(fdf2), load = fdf2[,f2])
      val2$Chunk = as.character(val2$Chunk)
      values = inner_join(val1, val2, by="Chunk")
      rs = cor(values[,2], values[,3], method="s")
      tmp = data.frame(Type = "Chunks", Model1 = f1, Model2 = f2, r = rs)
      tmp$Type = as.character(tmp$Type)
      tmp$Model1 = as.character(tmp$Model1)
      tmp$Model2 = as.character(tmp$Model2)
      cormat = bind_rows(cormat, tmp)
    }
  }
  ggplot(data = cormat, aes(x = Model1, y = Model2, fill = r)) +
    geom_tile() +
    scale_fill_gradient2(low = muted("red"), mid = "white", high = muted("blue"),
                         midpoint = 0, limits = c(-1, 1)) +
    geom_text(aes(label = round(r, digits = 2))) +
    labs(x = modelName1, y = modelName2, fill="Correlation") +
    facet_grid(. ~ Type)
}
