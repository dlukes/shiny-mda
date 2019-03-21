# Usage example:
# source("load_data.R")
# data <- loadData("results/2018-11-14_8f.RData")
# factors_clusters <- add_clusters(data$fdf, 10)
# cluster_2D_plot(factors_clusters, dim_x="GLS1", dim_y="GLS2", a=0.4)
# cluster_means_plot(factors_clusters, dims=c("GLS1", "GLS2", "GLS5"))
# cluster_info(factors_clusters, cluster=4, topn=4)

library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)

# pomocna funkce na jednotne vytvareni klastru
add_clusters <- function(factors, k=10) {
  set.seed(123)
  data <- select(factors, contains("GLS")) # pouze sloupce s dimenzemi
  kmeans_fit <- kmeans(data, centers=k, iter.max=50, nstart=10)
  mutate(factors, Cluster=as.factor(kmeans_fit$cluster))
}

# zobrazeni 2D grafu textů s čísly klastrů
# parametr "a" je vzdálenost textu od průměru daného klastru na dané dimenzi
# a pomáhá filtrovat data od odlehlých hodnot (při nižších hodnotách jsou líp vidět diskrétní klastry)
# a má smysl od 0.1 zhruba do 2
cluster_2D_plot <- function(factors_clusters, dim_x, dim_y, a=1) { # a - míra centrality zobrazených bodů
  data <- select(factors_clusters, dim_x, dim_y, Cluster) %>%
    group_by(Cluster) %>%
    mutate(MEDIAN_X=median(get(dim_x)), MEDIAN_Y=median(get(dim_y))) %>%
    filter(
      get(dim_x) > (MEDIAN_X - a),
      get(dim_x) < (MEDIAN_X + a),
      get(dim_y) > (MEDIAN_Y - a),
      get(dim_y) < (MEDIAN_Y + a)
    )
  ggplot(data, aes_string(x=dim_x, y=dim_y, color="Cluster", label="Cluster")) +
    geom_text(alpha=0.7, size=7)
}

# průměrná hodnota klastrů na dimenzích
# volitelný je seznam dimenzí na ose x
cluster_means_plot <- function(factors_clusters, dims) {
  data <- group_by(factors_clusters, Cluster) %>%
    summarize_if(is.numeric, mean) %>%
    select(dims, Cluster) %>%
    gather("Dimension", "Mean", -Cluster)
  ggplot(data, aes(x=Dimension, y=Mean, group=Cluster, color=Cluster, label=Cluster)) +
    geom_line() +
    geom_label(alpha=0.7, size=7)# TODO:, position=position_jitter(width=.1))
}

# výpis nejčastějších textových kategorií v klastru
# specifikuje se číslo klastru ("klastr") a počet vypisovaných kategorií ("topn") - nemusí sedět, pokud je poslední hodnota zastoupena v datech víckrát vypíšou se všechny instance
cluster_info <- function(factors_clusters, cluster, topn=5) {
  filter(factors_clusters, Cluster == cluster) %>%
    # we don't need MODE, it's already integrated in DIVISION
    select(-MODE) %>%
    unite(Category, DIVISION, SUPERCLASS, CLASS, sep="-") %>%
    count(Category, sort=TRUE) %>%
    mutate(`%`=percent(n / sum(n), accuracy=1)) %>%
    top_n(topn)
}

cluster_sizes <- function(factors_clusters) {
  count(factors_clusters, Cluster, sort=TRUE) %>%
    mutate(`%`=percent(n / sum(n), accuracy=1))
}
