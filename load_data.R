library(tibble)
library(tidyr)
library(dplyr)

# Load and preprocess data from an *.RData file in results/. Use data in the format returned by this
# function to develop new functionality.
loadData <- function(path) {
  env <- new.env()
  load(path, env)
  env <- as.list(env)

  orig <- env$res.data

  fdf <- env$factors
  ffactors <- grep("^(X|MODE|DIVISION|SUPERCLASS|CLASS)$", colnames(fdf), value=TRUE, invert=TRUE)
  fdf$DIVISION_ORIG <- as.factor(fdf$DIVISION)
  # expand division label to make it more intuitive
  fdf$DIVISION <- paste(as.character(fdf$MODE), as.character(fdf$DIVISION), sep="-")
  # coerce metadata to factors; reorder MODE and DIVISION levels so that any
  # additional non-Koditex data goes last to keep original visuals
  fdf$MODE <- as.factor(fdf$MODE) %>% orig_levels_first()
  fdf$DIVISION <- as.factor(fdf$DIVISION) %>% orig_levels_first()
  fdf$SUPERCLASS <- as.factor(fdf$SUPERCLASS)
  fdf$CLASS <- as.factor(fdf$CLASS)
  fdf$X <- row.names(fdf)
  modes <- levels(fdf$MODE)
  divisions <- levels(fdf$DIVISION)

  ldf <- unclass(env$load)
  ldf <- data.frame(Feature=row.names(ldf), ldf) %>%
    gather(Factor, Loading, -Feature, factor_key=TRUE)
  lfactors <- levels(ldf$Factor)

  meta <- rename(fdf, ID=X) %>%
    select(ID, MODE, DIVISION, SUPERCLASS, CLASS)
  # TODO: Some results don't contain the orig data frame (we didn't use
  # to need it until we started computing correlated features in the
  # loadings table, and we've since used it for other purposes as well).
  # Ideally, we should fix that, but until that happens, the following
  # if-expression will prevent us from failing when loading these older
  # data sets.
  norm <- if (is.null(orig)) {
    NULL
  } else {
    mutate(orig, ID=row.names(orig)) %>%
      mutate_if(is.numeric, funs(ntile(., n=100))) %>%
      left_join(meta, by="ID") %>%
      select(ID, MODE, DIVISION, SUPERCLASS, CLASS, everything()) %>%
      # TODO: use gather_if when available
      gather("FEAT", "PERCENTILE", colnames(select_if(., is.numeric)))
  }

  list(
    ldf=ldf, fdf=fdf, lfactors=lfactors, ffactors=ffactors, modes=modes, divisions=divisions,
    orig=orig, norm=norm
  )
}

orig_levels_first <- function(fct) {
  lvls <- levels(fct)
  orig <- grep("^(spo|web|wri)($|-)", lvls, value=TRUE)
  non_orig <- grep("^(spo|web|wri)($|-)", lvls, value=TRUE, invert=TRUE)
  factor(fct, levels=c(orig, non_orig))
}
