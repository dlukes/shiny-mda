# Load and preprocess data from an *.RData file in results/. Use data in the format returned by this
# function to develop new functionality.
loadData <- function(path) {
  env <- new.env()
  load(path, env)
  env <- as.list(env)

  fdf <- env$factors
  fdf$X <- row.names(fdf)
  ffactors <- grep("^(X|MODE|DIVISION|SUPERCLASS|CLASS)$", colnames(fdf), value=TRUE, invert=TRUE)
  fdf$DIVISION <- factor(
    fdf$DIVISION,
    c("int", "nin", "mul", "uni", "fic", "nfc", "nmg", "pri"),
    c("spo-int", "spo-nin", "web-mul", "web-uni", "wri-fic", "wri-nfc", "wri-nmg", "wri-pri")
  )
  modes <- levels(fdf$MODE)
  divisions <- levels(fdf$DIVISION)

  ldf <- unclass(env$load)
  ldf <- data.frame(Feature=row.names(ldf), ldf) %>%
    gather(Factor, Loading, -Feature, factor_key=TRUE)
  lfactors <- levels(ldf$Factor)

  list(ldf=ldf, fdf=fdf, lfactors=lfactors, ffactors=ffactors, modes=modes, division=divisions)
}
