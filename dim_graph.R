# Usage example:
# factors = read.table("results_fa/f1-f7d.csv", header=T, sep=",", row.names=1)
# DimDraw(factors, factor.name = "GLS3", low.perc = 0.2, up.perc = 0.8, col.palette = "Set3")

library(reshape2)
library(ggplot2)
library(pals)

cat2desc_df <- read.csv("./conf/cat2desc.csv")
cat2desc <- as.character(cat2desc_df$desc)
names(cat2desc) <- cat2desc_df$cat
rm(cat2desc_df)

DimDraw <- function(data, factor.name = "GLS1", low.perc = 0.3, up.perc = 0.7, col.palette = NULL) {
  data$CAT = paste(data$DIVISION, data$SUPERCLASS, data$CLASS, sep="-")
  data$Desc = addDesc(data$CAT)
  data.q = quantile(data[[factor.name]], c(low.perc, up.perc))
  data.melt = melt(data[c(factor.name, "CAT", "Desc")])
  data.melt$CAT = as.factor(data.melt$CAT)
  data.melt$Desc = as.factor(data.melt$Desc)
  selected = data.frame()
  selected.i = 1
  for (i in names(table(data.melt$CAT))) {
    if (median(data.melt[ data.melt$CAT == i,]$value) < data.q[1] |
        median(data.melt[ data.melt$CAT == i,]$value) > data.q[2]) {
      selected[selected.i,1] = i
      selected[selected.i,2] = median(data.melt[ data.melt$CAT == i,]$value)
      selected.i = selected.i + 1
    }
  }
  colnames(selected) = c("CAT", "Median")
  selected = selected[ order(selected$Median), ]
  # print(selected)
  # palette vector must be unnamed, else ggplot2 tries to use the names to match colors to values
  if (is.null(col.palette)) col.palette <- unname(polychrome(nrow(selected)))
  data.melt.sel = subset(data.melt, CAT %in% selected$CAT)
  data.melt.sel$CAT = droplevels(data.melt.sel$CAT)
  data.melt.sel$Desc = droplevels(data.melt.sel$Desc)
  data.melt.sel$CAT = factor(data.melt.sel$CAT, levels = selected$CAT)
  data.melt.sel$Desc = factor(data.melt.sel$Desc, levels = addDesc(selected$CAT))
  ggplot(data.melt.sel, aes(x = CAT, y = value)) +
    geom_boxplot(aes(fill = CAT)) + theme_bw(base_size = 16) +
    labs(x = "Metadata", y = "Factor value", title = paste("Scores for", factor.name)) +
    scale_fill_manual(
                     breaks = selected$CAT,
                     labels = addDesc(selected$CAT),
                     values = col.palette,
                     name = "Text categories") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

addDesc <- function(cat) {
  unname(cat2desc[as.character(cat)])
}
