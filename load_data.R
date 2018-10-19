library(tibble)
library(tidyr)

# Load and preprocess data from an *.RData file in results/. Use data in the format returned by this
# function to develop new functionality.
loadData <- function(path) {
  env <- new.env()
  load(path, env)
  env <- as.list(env)

  fdf <- env$factors
  # expand division label to make it more intuitive
  fdf$DIVISION <- paste(as.character(fdf$MODE), as.character(fdf$DIVISION), sep="-")
  # coerce metadata to factors
  fdf$MODE <- as.factor(fdf$MODE)
  fdf$DIVISION <- as.factor(fdf$DIVISION)
  fdf$SUPERCLASS <- as.factor(fdf$SUPERCLASS)
  fdf$CLASS <- as.factor(fdf$CLASS)
  # reorder levels so that cpact* goes last to keep original visuals
  fdf$MODE <- orig_levels_first(fdf$MODE)
  fdf$DIVISION <- orig_levels_first(fdf$DIVISION)
  fdf$X <- row.names(fdf)
  ffactors <- grep("^(X|MODE|DIVISION|SUPERCLASS|CLASS)$", colnames(fdf), value=TRUE, invert=TRUE)
  modes <- levels(fdf$MODE)
  divisions <- levels(fdf$DIVISION)

  ldf <- unclass(env$load)
  ldf <- data.frame(Feature=row.names(ldf), ldf) %>%
    gather(Factor, Loading, -Feature, factor_key=TRUE)
  lfactors <- levels(ldf$Factor)

  feat_crit_table <- data.frame(env$fit$loadings[,], h2 = env$fit$communality, comp = env$fit$complexity)
  feat_crit_table <- rownames_to_column(feat_crit_table, var="Feature")

  list(
    ldf=ldf, fdf=fdf, lfactors=lfactors, ffactors=ffactors, modes=modes, divisions=divisions,
    orig=env$res.data, feat_crit_table=feat_crit_table
  )
}

orig_levels_first <- function(fct) {
  lvls <- levels(fct)
  orig <- grep("^(spo|web|wri)($|-)", lvls, value=TRUE)
  non_orig <- grep("^(spo|web|wri)($|-)", lvls, value=TRUE, invert=TRUE)
  factor(fct, levels=c(orig, non_orig))
}
