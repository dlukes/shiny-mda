# Usage example:
# source("load_data.R")
# model1 <- "2017-12-05_8f.RData"
# test <- loadData(file.path("results", model1))
# add_correlated_feats(test$ldf, test$orig) %>%
#   filter(!is.na(CorrelatedWith))

library(tidyverse)

add_correlated_feats <- function(ldf, orig, cor_thresh=c(-.95, .95)) {
  as.data.frame(cor(orig, method="spearman")) %>%
    rownames_to_column(var="Feature") %>%
    gather(CorrelatedWith, Cor, -Feature) %>%
    filter(Cor < 1 & (Cor < cor_thresh[1] | Cor > cor_thresh[2])) %>%
    mutate(CorrelatedWith=sprintf("%s (%.2f)", CorrelatedWith, Cor)) %>%
    select(-Cor) %>%
    group_by(Feature) %>%
    summarize(CorrelatedWith=paste(CorrelatedWith, collapse=", ")) %>%
    right_join(ldf)
}
