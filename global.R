library(jsonlite)
library(ggplot2)
library(Cairo)  # For nicer ggplot2 output when deployed on Linux

# load tidyverse last to make sure it overrides all conflicting function names
library(tidyverse)

enableBookmarking("server")

palette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7",
             "green", "blue", "red", "yellow")
shapes <- c(16, 17, 15, 18)
lines <- c("solid", "longdash", "dotted", "dotdash")
theme_set(theme_bw(base_size=18))
feat2desc <- read_csv("./conf/feat2desc.csv")
globalMeta <- read_delim("./conf/koditex-metadata.csv", delim="\t")

lsResults <- function() {
  results <- sort(list.files("results", pattern="\\.RData$", full.names=TRUE), decreasing=TRUE)
  names(results) <- tools::file_path_sans_ext(basename(results))
  results
}
