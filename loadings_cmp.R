# Usage example:
# source("load_data.R")
# source("global.R")
# feat2desc <- select(feat2desc, Feature, Description)
# ref <- loadData(file.path("results", "2017-12-05_8f.RData"))
# cmp <- loadData(file.path("results", "2018-11-14_8f.RData"))
# compare_loadings(ref$ldf, cmp$ldf, "GLS1", "GLS1", feat2desc, threshold=c(-.5, .5))

compare_loadings <- function(ldfA, ldfB, facA, facB, feat2desc=NULL, threshold=c(-.3, .3), only_feats_in_both=FALSE) {
  ldf <- full_join(
    filter(ldfA, Factor == facA),
    filter(ldfB, Factor == facB),
    by="Feature",
    suffix=c("A", "B")
  ) %>%
    mutate(Agreement=sign(LoadingA * LoadingB) *(1 - abs(abs(LoadingA) - abs(LoadingB))))
  if (!is.null(feat2desc)) {
    ldf <- left_join(ldf, feat2desc)
  }
  ldf <- select(ldf, Feature, Description, LoadingA, LoadingB, Agreement)
  both <- filter(ldf,
    (LoadingA <= threshold[1] | LoadingA >= threshold[2])
    &
    (LoadingB <= threshold[1] | LoadingB >= threshold[2])
  ) %>%
    arrange(desc(Agreement))
  onlyA <- filter(ldf,
    (LoadingA <= threshold[1] | LoadingA >= threshold[2])
    &
    (!(LoadingB <= threshold[1] | LoadingB >= threshold[2]) | is.na(LoadingB))
  ) %>%
    arrange(Agreement)
  onlyB <- filter(ldf,
    (!(LoadingA <= threshold[1] | LoadingA >= threshold[2]) | is.na(LoadingA))
    &
    (LoadingB <= threshold[1] | LoadingB >= threshold[2])
  ) %>%
    arrange(Agreement)
  if (only_feats_in_both) {
    onlyA <- drop_na(onlyA)
    onlyB <- drop_na(onlyB)
  }
  list(both=both, onlyA=onlyA, onlyB=onlyB)
}
