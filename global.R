library(jsonlite)
library(ggplot2)
library(Cairo)  # For nicer ggplot2 output when deployed on Linux

# load tidyverse last to make sure it overrides all conflicting function names
library(tidyverse)

enableBookmarking("server")

palette <- c(
  "#191919",
  "#ffa405",
  "#56b4e9",
  "#00998f",
  "#ffe100",
  "#0075dc",
  "#ff5005",
  "#f0a3ff",

  "#426600",
  "#003380",
  "#ff0010",
  "#ffff80",

  "#94ffb5",
  "#808080",
  "#990000",
  "#e0ff66",
  "#c20088",
  "#8f7c00",
  "#993f00",
  "#ffa8bb",
  "#005c31",
  "#4c005c",
  "#ffcc99",
  "#9dcc00",
  "#740aff",
  "#2bce48"
)

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
