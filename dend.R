# Usage examples are commented out.

library(tibble)
library(tidyr)
library(dplyr)
library(ggplot2)
library(ggdendro)

# === vstupni data modelu ===
# potrebujeme: factors, weights

# setwd("~/edu/mda")
# load("~/edu/mda/shiny/mda/results/2017-12-05_8f.RData")
# w <- fit.fa$Vaccounted[2,]
# w <- w / sum(w)
# rm(fit.fa, res.data, load)

# === vytvoreni dendrogramu + prirazeni metadat k textum v dendrogramu ===
vytvor_dendrogram <- function(factorscores, dim.weights = c()) {
  results <- list()
  tmp.dist <- factorscores %>% select(contains("GLS")) %>% dist() %>% as.matrix()
  if(is.vector(dim.weights)) {
    m <- factorscores %>% select(contains("GLS")) %>% distances::distances(weights = dim.weights) %>% as.matrix()
  } else {
    m <- factorscores %>% select(contains("GLS")) %>% distances::distances() %>% as.matrix()
  }
  dimnames(m) <- dimnames(tmp.dist)
  dend <- as.dist(m) %>% hclust %>% as.dendrogram()
  dend_data <- dendro_data(dend, type = "rectangle")
  id.category <- left_join(
    dend_data$labels,
    rownames_to_column(factorscores, var = "label") %>% select(label, MODE, DIVISION, SUPERCLASS, CLASS),
    by = "label"
  )
  line.category <- left_join(
    dend_data$segments,
    select(id.category, -y),
    by = "x"
  )
  results$id <- id.category
  results$line <- line.category
  results$dend <- dend
  return(results)
}

# dend.category <- vytvor_dendrogram(factors, w) # pokud není zadáno w, bude to bez vážení dimenzí

# === nakresli dendrogram ===
nakresli_dendrogram <- function(dend.category) {
  ggplot(dend.category$line) +
    geom_segment(aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_point(data = dend.category$id, aes(x, y, color = DIVISION)) +
    geom_text(data = dend.category$id,
              aes(x, y-0.1, label = paste(MODE,DIVISION,CLASS, sep="-"), color = DIVISION), hjust = 1, angle = 90, size = 3) +
    theme_classic()
}

# dendroplot <- nakresli_dendrogram(dend.category)
# print(dendroplot)

# === porcovani dendrogramu ===
# potrebujeme: celkovy pocet skupin (ng), prip. cislo konkretni skupiny pro detailni informaci (tg)

# numberOfGroups = 8  #(ng)
# targetGroup = 2     #(tg)

# === shrnutí pro dany pocet skupin ===
clust_summary <- function(dend.category, k = 0, h = 0, metadata = c("MODE", "DIVISION", "CLASS")) {
  results <- list()
  entro <- c()
  sums <- c()
  if (k != 0) {
    text.groups <- left_join(
      dend.category$id,
      as.data.frame(dendextend::cutree(dend.category$dend, k = k)) %>% rownames_to_column(var = "label") %>% rename(group = 2),
      by = "label"
    )
  } else {
    text.groups <- left_join(
      dend.category$id,
      as.data.frame(dendextend::cutree(dend.category$dend, h = h)) %>% rownames_to_column(var = "label") %>% rename(group = 2),
      by = "label"
    )
  }
  for (i in 1:max(text.groups$group)) {
    out <- filter(text.groups, group == i) %>% group_by_(.dots = metadata) %>% count() %>% arrange(desc(n))
    sums <- c(sums, sum(out$n))
    entro <- c( entro, -sum((out$n / sum(out$n)) * log2((out$n / sum(out$n)))) )
    results[[i]] <- as.data.frame(out)
  }
  # print(sum(entro))
  results$sums <-  sums
  results$entro <- entro
  return(results)
}

# summary <- clust_summary(dend.category, numberOfGroups, metadata = c("MODE", "DIVISION")) # vrací celkovou entropii
# summary$sums   # pocet textů v jednotlivých klastrech
# summary$entro  # entropie jednoltivých klastrů
# summary        # obsah jednoltivých klastrů

# === oznaceni skupiny v dendrogramu ===
vyznac_skupinu <- function(dend.category, ng, tg) {
  text.groups <- left_join(
    dend.category$id,
    as.data.frame(dendextend::cutree(dend.category$dend, k = ng)) %>% rownames_to_column(var = "label") %>% rename(group = 2),
    by = "label"
  )
  rozsah <- group_by(text.groups, group) %>% summarise(min = min(x), max = max(x)) %>% filter(group == tg)
  dendroplot <- nakresli_dendrogram(dend.category)
  p <- dendroplot +
    geom_rect(data = rozsah,
              aes(xmin = min, xmax = max, ymin = 0, ymax = 1.01*max(dend.category$line$y)), fill = "red", alpha = 0.2)
  return(p)
}

# dendroHighlighted <- vyznac_skupinu(dend.category, numberOfGroups, targetGroup)
# print(dendroHighlighted)

# === textovy vypis metadat pro danou skupinu ===
vypis_skupinu <- function(dend.category, ng, tg, metadata = c("MODE", "DIVISION", "CLASS", "id")) {
  text.groups <- left_join(
    dend.category$id,
    as.data.frame(dendextend::cutree(dend.category$dend, k = ng)) %>% rownames_to_column(var = "label") %>% rename(group = 2),
    by = "label"
  ) %>% rename(id = label)
  filter(text.groups, group == tg) %>% group_by_(.dots = metadata) %>% count()
}

# vypis_skupinu(dend.category, numberOfGroups, targetGroup, metadata = c("MODE", "DIVISION"))
