# Usage example:
# factors = read.table("results_fa/f1-f7d.csv", header=T, sep=",", row.names=1)
# DimDraw(factors, factor.name = "GLS3", low.perc = 0.2, up.perc = 0.8, col.palette = "Set3")

library(reshape2)
library(ggplot2)
library(pals)

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
  desc = data.frame(CAT = cat, description = rep("-", length(cat)))
  desc$description = as.character(desc$description)
  if (nrow(desc[ desc$CAT == "spo-int--bru", ]) > 0) { desc[ desc$CAT == "spo-int--bru", ]$description = "Broadcast discussion" }
  if (nrow(desc[ desc$CAT == "spo-int--eli", ]) > 0) { desc[ desc$CAT == "spo-int--eli", ]$description = "Elicited speech" }
  if (nrow(desc[ desc$CAT == "spo-int--inf", ]) > 0) { desc[ desc$CAT == "spo-int--inf", ]$description = "Private conversation" }
  if (nrow(desc[ desc$CAT == "spo-nin--wbs", ]) > 0) { desc[ desc$CAT == "spo-nin--wbs", ]$description = "Written-to-be-spoken" }
  if (nrow(desc[ desc$CAT == "web-mul--dis", ]) > 0) { desc[ desc$CAT == "web-mul--dis", ]$description = "Web discussions" }
  if (nrow(desc[ desc$CAT == "web-mul--fcb", ]) > 0) { desc[ desc$CAT == "web-mul--fcb", ]$description = "Facebook posts" }
  if (nrow(desc[ desc$CAT == "web-mul--for", ]) > 0) { desc[ desc$CAT == "web-mul--for", ]$description = "Web forums" }
  if (nrow(desc[ desc$CAT == "web-uni--blo", ]) > 0) { desc[ desc$CAT == "web-uni--blo", ]$description = "Blogs" }
  if (nrow(desc[ desc$CAT == "web-uni--wik", ]) > 0) { desc[ desc$CAT == "web-uni--wik", ]$description = "Wikipedia" }
  if (nrow(desc[ desc$CAT == "wri-fic--col", ]) > 0) { desc[ desc$CAT == "wri-fic--col", ]$description = "Short stories" }
  if (nrow(desc[ desc$CAT == "wri-fic-nov-crm", ]) > 0) { desc[ desc$CAT == "wri-fic-nov-crm", ]$description = "Crime novels" }
  if (nrow(desc[ desc$CAT == "wri-fic-nov-fan", ]) > 0) { desc[ desc$CAT == "wri-fic-nov-fan", ]$description = "Fantasy novels" }
  if (nrow(desc[ desc$CAT == "wri-fic-nov-gen", ]) > 0) { desc[ desc$CAT == "wri-fic-nov-gen", ]$description = "General fiction" }
  if (nrow(desc[ desc$CAT == "wri-fic-nov-lov", ]) > 0) { desc[ desc$CAT == "wri-fic-nov-lov", ]$description = "Romance novels" }
  if (nrow(desc[ desc$CAT == "wri-fic-nov-scf", ]) > 0) { desc[ desc$CAT == "wri-fic-nov-scf", ]$description = "Sci-fi novels" }
  if (nrow(desc[ desc$CAT == "wri-fic--scr", ]) > 0) { desc[ desc$CAT == "wri-fic--scr", ]$description = "Screenplays" }
  if (nrow(desc[ desc$CAT == "wri-fic--ver", ]) > 0) { desc[ desc$CAT == "wri-fic--ver", ]$description = "Poetry, songs" }
  if (nrow(desc[ desc$CAT == "wri-nfc--adm", ]) > 0) { desc[ desc$CAT == "wri-nfc--adm", ]$description = "Administrative texts" }
  if (nrow(desc[ desc$CAT == "wri-nfc--enc", ]) > 0) { desc[ desc$CAT == "wri-nfc--enc", ]$description = "Encyclopedias" }
  if (nrow(desc[ desc$CAT == "wri-nfc--mem", ]) > 0) { desc[ desc$CAT == "wri-nfc--mem", ]$description = "Memoirs" }
  if (nrow(desc[ desc$CAT == "wri-nfc-pop-fts", ]) > 0) { desc[ desc$CAT == "wri-nfc-pop-fts", ]$description = "POP: Tech. sc." }
  if (nrow(desc[ desc$CAT == "wri-nfc-pop-hum", ]) > 0) { desc[ desc$CAT == "wri-nfc-pop-hum", ]$description = "POP: Humanities" }
  if (nrow(desc[ desc$CAT == "wri-nfc-pop-nat", ]) > 0) { desc[ desc$CAT == "wri-nfc-pop-nat", ]$description = "POP: Natural sc." }
  if (nrow(desc[ desc$CAT == "wri-nfc-pop-ssc", ]) > 0) { desc[ desc$CAT == "wri-nfc-pop-ssc", ]$description = "POP: Social sc." }
  if (nrow(desc[ desc$CAT == "wri-nfc-pro-fts", ]) > 0) { desc[ desc$CAT == "wri-nfc-pro-fts", ]$description = "PRO: Tech. sc." }
  if (nrow(desc[ desc$CAT == "wri-nfc-pro-hum", ]) > 0) { desc[ desc$CAT == "wri-nfc-pro-hum", ]$description = "PRO: Humanities" }
  if (nrow(desc[ desc$CAT == "wri-nfc-pro-nat", ]) > 0) { desc[ desc$CAT == "wri-nfc-pro-nat", ]$description = "PRO: Natural sc." }
  if (nrow(desc[ desc$CAT == "wri-nfc-pro-ssc", ]) > 0) { desc[ desc$CAT == "wri-nfc-pro-ssc", ]$description = "PRO: Social sc." }
  if (nrow(desc[ desc$CAT == "wri-nfc-sci-fts", ]) > 0) { desc[ desc$CAT == "wri-nfc-sci-fts", ]$description = "SCI: Tech. sc." }
  if (nrow(desc[ desc$CAT == "wri-nfc-sci-hum", ]) > 0) { desc[ desc$CAT == "wri-nfc-sci-hum", ]$description = "SCI: Humanities" }
  if (nrow(desc[ desc$CAT == "wri-nfc-sci-nat", ]) > 0) { desc[ desc$CAT == "wri-nfc-sci-nat", ]$description = "SCI: Natural sc." }
  if (nrow(desc[ desc$CAT == "wri-nfc-sci-ssc", ]) > 0) { desc[ desc$CAT == "wri-nfc-sci-ssc", ]$description = "SCI: Social sc." }
  if (nrow(desc[ desc$CAT == "wri-nmg-lei-hou", ]) > 0) { desc[ desc$CAT == "wri-nmg-lei-hou", ]$description = "Hobby mag." }
  if (nrow(desc[ desc$CAT == "wri-nmg-lei-int", ]) > 0) { desc[ desc$CAT == "wri-nmg-lei-int", ]$description = "Interest. facts mag." }
  if (nrow(desc[ desc$CAT == "wri-nmg-lei-lif", ]) > 0) { desc[ desc$CAT == "wri-nmg-lei-lif", ]$description = "Lifestyle mag." }
  if (nrow(desc[ desc$CAT == "wri-nmg-lei-mix", ]) > 0) { desc[ desc$CAT == "wri-nmg-lei-mix", ]$description = "Misc. mag." }
  if (nrow(desc[ desc$CAT == "wri-nmg-lei-sct", ]) > 0) { desc[ desc$CAT == "wri-nmg-lei-sct", ]$description = "Tabloids" }
  if (nrow(desc[ desc$CAT == "wri-nmg-lei-spo", ]) > 0) { desc[ desc$CAT == "wri-nmg-lei-spo", ]$description = "Sport mag." }
  if (nrow(desc[ desc$CAT == "wri-nmg-new-com", ]) > 0) { desc[ desc$CAT == "wri-nmg-new-com", ]$description = "Op-eds, columns" }
  if (nrow(desc[ desc$CAT == "wri-nmg-new-cul", ]) > 0) { desc[ desc$CAT == "wri-nmg-new-cul", ]$description = "Culture news" }
  if (nrow(desc[ desc$CAT == "wri-nmg-new-eco", ]) > 0) { desc[ desc$CAT == "wri-nmg-new-eco", ]$description = "Economic news" }
  if (nrow(desc[ desc$CAT == "wri-nmg-new-fre", ]) > 0) { desc[ desc$CAT == "wri-nmg-new-fre", ]$description = "Leisure time art." }
  if (nrow(desc[ desc$CAT == "wri-nmg-new-pol", ]) > 0) { desc[ desc$CAT == "wri-nmg-new-pol", ]$description = "Politics news" }
  if (nrow(desc[ desc$CAT == "wri-nmg-new-rep", ]) > 0) { desc[ desc$CAT == "wri-nmg-new-rep", ]$description = "News" }
  if (nrow(desc[ desc$CAT == "wri-pri--cor", ]) > 0) { desc[ desc$CAT == "wri-pri--cor", ]$description = "Letters" }
  return(desc$description)
}
