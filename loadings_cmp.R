# Usage example:
# source("load_data.R")
# source("global.R")
# feat2desc <- select(feat2desc, Feature, Description)
# ref <- loadData(file.path("results", "2017-12-05_8f.RData"))
# cmp <- loadData(file.path("results", "2018-11-14_8f.RData"))
# compare_loadings(ref$ldf, cmp$ldf, "GLS1", "GLS1", feat2desc, threshold=c(-.5, .5))

filter_load <- function(loadings, factor, threshold) {
  filter(loadings, Factor == factor & (Loading <= threshold[1] | Loading >= threshold[2])) %>%
    select(-Factor)
}

compare_loadings <- function(ldfA, ldfB, facA, facB, feat2desc, threshold=c(-.3, .3), only_feats_in_both=FALSE) {
  featsA <- unique(ldfA$Feature)
  featsB <- unique(ldfB$Feature)
  if (!is.null(feat2desc)) {
    ldfA <- left_join(ldfA, feat2desc)
    ldfB <- left_join(ldfB, feat2desc)
  }
  loadA <- filter_load(ldfA, facA, threshold)
  loadB <- filter_load(ldfB, facB, threshold)
  both <- inner_join(loadA, loadB, by="Feature", suffix=c("A", "B")) %>%
    mutate(Agreement=1 - abs(abs(LoadingA) - abs(LoadingB))) %>%
    arrange(desc(Agreement)) %>%
    select(-DescriptionB) %>%
    rename(Description=DescriptionA) %>%
    select(Feature, Description, everything())
  onlyA <- anti_join(loadA, loadB, by="Feature") %>%
    arrange(desc(Loading)) %>%
    select(Feature, Description, everything()) %>%
    mutate(InOtherModel=Feature %in% featsB)
  onlyB <- anti_join(loadB, loadA, by="Feature") %>%
    arrange(desc(Loading)) %>%
    select(Feature, Description, everything()) %>%
    mutate(InOtherModel=Feature %in% featsA)
  if (only_feats_in_both) {
    onlyA <- filter(onlyA, InOtherModel == TRUE)
    onlyB <- filter(onlyB, InOtherModel == TRUE)
  }
  list(both=both, onlyA=onlyA, onlyB=onlyB)
}
