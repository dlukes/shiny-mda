# Usage example:
# source("load_data.R")
# feat2desc <- read_csv("conf/feat2desc_cs.csv")
# data <- loadData("results/2018-11-14_8f.RData")
# lst <- top_feature_boxplot(data$norm, data$ldf, feat2desc, chunk_id="_metodickyp_0")
# print(lst$plot)

library(ggplot2)
library(dplyr)

feats_above_thresh <- function(ldf, dimname, thresh, feat2desc) {
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

top_feature_boxplot <- function(norm_df, ldf, feat2desc, dimname="GLS1", thresh=c(-3, .3), chunk_id="", meta_regex="") {
  chunk_id <- trimws(chunk_id)
  meta_regex <- trimws(meta_regex)
  top_feats <- feats_above_thresh(ldf, dimname, thresh, feat2desc)
  full_df <- filter(norm_df, FEAT %in% top_feats$Feature) %>%
    mutate(FEAT=factor(FEAT, levels=top_feats$Feature, labels=top_feats$Label))
  if (meta_regex == "") {
    box_df <- full_df
    group_column <- "DIVISION"
  } else {
    # don't paste MODE here at the beginning, it's already pasted at the
    # beginning of DIVISION
    box_df <- mutate(full_df, MERGED_META=paste(DIVISION, SUPERCLASS, CLASS, sep="-")) %>%
      filter(grepl(meta_regex, MERGED_META))
    group_column <- "MERGED_META"
  }
  plot <- ggplot(box_df, aes_string(group_column, "PERCENTILE", fill=group_column)) +
    geom_boxplot(alpha=.5) +
    facet_wrap(~FEAT, labeller=label_wrap_gen(30)) +
    coord_flip() +
    scale_fill_manual(values=palette, drop=FALSE)
  if (chunk_id != "") {
    plot <- plot +
      geom_hline(aes(yintercept=PERCENTILE), filter(full_df, ID == chunk_id), color="red")
  }
  # nrow is needed to compute a sensible height for Shiny to render the
  # plot
  list(plot=plot, nrow=wrap_dims(nrow(top_feats))[1])
}
