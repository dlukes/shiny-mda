# Usage example:
# source("load_data.R")
# model1 = "2017-09-18_4f.RData"
# ref = loadData(file.path("results", model1))
# model2 = "2017-05-18.RData"
# cmp = loadData(file.path("results", model2))
# cormat = bind_rows(
#   modelCmpFeatures(ref$ldf, cmp$ldf),
#   modelCmpChunks(ref$fdf, cmp$fdf)
# )
# plotModelCmp(cormat, model1, model2)
#
# modelNames <- c(
#   "2017-05-18.RData",
#   "2017-09-18_8f.RData",
#   "2017-09-18_7f.RData",
#   "2017-09-18_6f.RData",
#   "2017-09-18_5f.RData",
#   "2017-09-18_4f.RData"
# )
# models <- vector("list", length(modelNames))
# names(models) <- modelNames
# for (modelName in modelNames) {
#   model <- loadData(file.path("results", modelName))
#   models[[modelName]] <- model
# }
# df <- multiModelCmp(models, c("VYPW", "KONT"))
# plotMultiModelCmp(df, featSet=TRUE)

library(ggplot2)
library(tidyr)
library(dplyr)
library(scales)

initCormat <- function() {
  cormat = data.frame(Type = vector(mode="character", length=0),
                      Model1 = vector(mode="character", length=0),
                      Model2 = vector(mode="character", length=0),
                      r = vector(mode="logical", length=0))
  cormat$Type = as.character(cormat$Type)
  cormat$Model1 = as.character(cormat$Model1)
  cormat$Model2 = as.character(cormat$Model2)
  cormat
}

modelCmpFeatures <- function(ldf1, ldf2) {
  cormat = initCormat()
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
  cormat
}

modelCmpChunks <- function(fdf1, fdf2) {
  fdf1 = select(fdf1, -MODE, -DIVISION, -SUPERCLASS, -CLASS, -X)
  fdf2 = select(fdf2, -MODE, -DIVISION, -SUPERCLASS, -CLASS, -X)
  cormat = initCormat()
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
  cormat
}

modelCmpFeatSet <- function(ldf1, ldf2, featSet) {
  # in this case, it's not really a correlation matrix, but let's keep the variable name anyway
  cormat = initCormat()
  details = tibble(Feature=character(), Factors=character(), LoadProd=numeric())
  ldf1 <- filter(ldf1, Feature %in% featSet)
  ldf2 <- filter(ldf2, Feature %in% featSet)
  for (f1 in levels(ldf1$Factor)) {
    val1 = filter(ldf1, Factor == f1) %>% select(-Factor)
    val1$Feature = as.character(val1$Feature)
    for (f2 in levels(ldf2$Factor)) {
      val2 = filter(ldf2, Factor == f2) %>% select(-Factor)
      val2$Feature = as.character(val2$Feature)
      values = inner_join(val1, val2, by="Feature")
      # not a correlation measure
      pw_prod = values[,2] * values[,3]
      rs = mean(pw_prod)
      tmp = data.frame(Type = "Features", Model1 = f1, Model2 = f2, r = rs)
      tmp$Type = as.character(tmp$Type)
      tmp$Model1 = as.character(tmp$Model1)
      tmp$Model2 = as.character(tmp$Model2)
      cormat = bind_rows(cormat, tmp)
      details <- bind_rows(
        details,
        tibble(Feature=values$Feature, Factors=sprintf("%s→%s", f1, f2), LoadProd=pw_prod)
      )
    }
  }
  list(cormat=cormat, details=details)
}

plotModelCmp <- function(cormat, modelName1, modelName2) {
  ggplot(data = cormat, aes(x = Model1, y = Model2, fill = r)) +
    geom_tile() +
    scale_fill_gradient2(low = muted("red"), mid = "white", high = muted("blue"),
                         midpoint = 0, limits = c(-1, 1)) +
    geom_text(aes(label = round(r, digits = 2))) +
    labs(x = modelName1, y = modelName2, fill="Correlation") +
    facet_grid(. ~ Type)
}

multiModelCmp <- function(models, featSet=NULL) {
  numCmps <- length(models) - 1
  modelNames <- names(models)
  modelCmps <- vector("list", numCmps)
  detailsList <- vector("list", numCmps)
  for (i in 1:numCmps) {
    model1 <- models[[i]]
    model2 <- models[[i+1]]
    modelName1 <- modelNames[i]
    modelName2 <- modelNames[i+1]
    if (is.null(featSet)) {
      modelCmp <- modelCmpFeatures(model1, model2)
      details <- NULL
    } else {
      res <- modelCmpFeatSet(model1, model2, featSet)
      modelCmp <- res$cormat
      details <- res$details
    }
    modelCmp$ModelName1 <- modelName1
    modelCmp$ModelName2 <- modelName2
    if (!is.null(details)) {
      details$ModelName1 <- modelName1
      details$ModelName2 <- modelName2
    }
    modelCmps[[i]] <- modelCmp
    detailsList[[i]] <- details
  }
  modelDf <- bind_rows(modelCmps)
  modelDf$Model1 <- gsub("\\D", "", modelDf$Model1)
  modelDf$Model2 <- gsub("\\D", "", modelDf$Model2)
  modelDf$ModelName1 <- factor(modelDf$ModelName1, levels=modelNames)
  modelDf$ModelName2 <- factor(modelDf$ModelName2, levels=modelNames)
  detailsDf <- bind_rows(detailsList)
  list(models=modelDf, details=detailsDf)
}

plotMultiModelCmp <- function(data, featSet=FALSE) {
  colorLabel <- if (featSet) {
    "Mean point-\nwise product"
  } else {
    "Correlation"
  }
  ggplot(data=data, aes(x=ModelName1, y=Model1)) +
    geom_segment(aes(xend=ModelName2, yend=Model2, size=abs(r) , color=r, alpha=abs(r))) +
    scale_x_discrete(drop=FALSE) +
    scale_color_gradient2(
      low=muted("red"), mid="white", high=muted("blue"), midpoint=0, limits=c(-1, 1)
    ) +
    scale_size_continuous(range=c(0, 1)) +
    theme(panel.grid=element_blank()) +
    labs(x="Models", y="Dimensions", color=colorLabel) +
    guides(size="none", alpha="none")
}
