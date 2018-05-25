# Usage example:
# source("load_data.R")
# model <- "2017-12-05_8f.RData"
# test <- loadData(file.path("results", model))
# plotFeatCrit(test$feat_crit_table, "GLS1")

library(ggplot2)

plotFeatCrit <- function(feat_crit_table, dim) {
  ggplot(data=filter(feat_crit_table, abs(round(get(dim), 2)) >= 0.3),
         aes(x=get(dim), y=comp, color=h2, label=Feature)) +
    geom_vline(aes(xintercept=0), color="darkgrey") +
    geom_vline(aes(xintercept=0.3), linetype=2, color="darkgrey") +
    geom_vline(aes(xintercept=-0.3), linetype=2, color="darkgrey") +
    geom_label(position="jitter", size=3) +
    scale_color_gradient(low="red", high="blue") +
    coord_cartesian(ylim=c(1, max(feat_crit_table$comp)), xlim=c(-1, 1)) +
    labs(x=paste("Loading on", dim), y="Complexity", color="Communality")
}
