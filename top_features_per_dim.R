# Usage example:
# source("load_data.R")
# data <- loadData("results/2018-11-14_8f.RData")
# lst <- top_feature_boxplot(data$norm, data$ldf, chunk_id="_metodickyp_0")
# print(lst$plot)

feats_above_thresh <- function(ldf, dimname, thresh) {
  filter(ldf, Factor == dimname & (Loading < thresh[1] | Loading > thresh[2])) %>%
    mutate(Feature=as.character(Feature)) %>%
    arrange(desc(Loading)) %>%
    left_join(feat2desc, by="Feature") %>%
    select(Feature, Description, Loading) %>%
    mutate(
      Label=paste0(
        ifelse(Loading > 0, "+ ", "- "),
        as.character(Description),
        " (",
        signif(Loading, 2),
        ")"
      )
    ) %>%
    as_tibble()
}

top_feature_boxplot <- function(norm_df, ldf, dimname="GLS1", thresh=c(-3, .3), chunk_id=NULL) {
  top_feats <- feats_above_thresh(ldf, dimname, thresh)
  df <- filter(norm_df, FEAT %in% top_feats$Feature) %>%
    mutate(FEAT=factor(FEAT, levels=top_feats$Feature, labels=top_feats$Label))
  plot <- ggplot(df, aes(DIVISION, PERCENTILE, fill=DIVISION)) +
    geom_boxplot(alpha=.5) +
    facet_wrap(~FEAT, labeller=label_wrap_gen(30)) +
    coord_flip() +
    scale_fill_manual(values=palette, drop=FALSE)
  if (!is.null(chunk_id)) {
    plot <- plot +
      geom_hline(aes(yintercept=PERCENTILE), filter(df, ID == chunk_id), color="red")
  }
  # nrow is needed to compute a sensible height for Shiny to render the
  # plot
  list(plot=plot, nrow=wrap_dims(nrow(top_feats))[1])
}
