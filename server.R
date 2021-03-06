source("load_data.R", local=TRUE)
source("dim_graph.R", local=TRUE)
source("genre_diff.R", local=TRUE)
source("model_cmp.R", local=TRUE)
source("loadings_cmp.R", local=TRUE)
source("filterRange_override.R", local=TRUE)
source("correlated_feats.R", local=TRUE)
source("feat_crit.R", local=TRUE)
source("top_features_per_dim.R", local=TRUE)
source("text_types.R", local=TRUE)

ltable_js <- DT::JS(read_file("./www/loadingsTable.js"))
ltable_state_default <- DT::JS("function(settings, data) { return false; }")

null2empty_list <- function(inp) {
  if (is.null(inp)) {
    return(list())
  } else {
    return(inp)
  }
}

# The following two functions encapsulate all changes to the UI that are
# driven by input data, which means they need special treatment w.r.t.
# bookmarking.

dataDrivenUIUpdate <- function(
  session, mode, division, fx, fy, showfactors, dimA, feat_crit_dim,
  cluster_2D_x, cluster_2D_y, cluster_means_dims
) {
  # NOTE: When no option is selected in a checkbox group, Shiny represents
  # this as NULL. Unfortunately, the update functions below simply ignore
  # NULL arguments, so we have to manually convert possible NULLs into
  # empty lists. This is necessary for bookmarking to work properly.
  selected_modes <- null2empty_list(mode[[2]])
  updateCheckboxGroupInput(session, "mode", choices=mode[[1]], selected=selected_modes)
  selected_divisions <- null2empty_list(division[[2]])
  updateCheckboxGroupInput(session, "division", choices=division[[1]], selected=selected_divisions)
  updateSelectInput(session, "fx", choices=fx[[1]], selected=fx[[2]])
  updateSelectInput(session, "fy", choices=fy[[1]], selected=fy[[2]])
  selected_showfactors <- null2empty_list(showfactors[[2]])
  updateCheckboxGroupInput(session, "showfactors", choices=showfactors[[1]], selected=selected_showfactors)
  updateSelectInput(session, "dimA", choices=dimA[[1]], selected=dimA[[2]])
  updateSelectInput(session, "feat_crit_dim", choices=feat_crit_dim[[1]], selected=feat_crit_dim[[2]])
  updateSelectInput(session, "cluster_2D_x", choices=cluster_2D_x[[1]], selected=cluster_2D_x[[2]])
  updateSelectInput(session, "cluster_2D_y", choices=cluster_2D_y[[1]], selected=cluster_2D_y[[2]])
  updateSelectizeInput(session, "cluster_means_dims", choices=cluster_means_dims[[1]], selected=cluster_means_dims[[2]])
}

cmpDataDrivenUIUpdate <- function(session, dimB) {
  updateSelectInput(session, "dimB", choices=dimB[[1]], selected=dimB[[2]])
}

function(input, output, session) {
  ###################################################################################################
  # INPUT DEBOUNCING

  model_seq_d <- debounce(reactive(input$model_seq), 1000)
  mmc_feat_set_d <- debounce(reactive(input$mmc_feat_set), 1000)
  mmc_feats_from_mod_d <- debounce(reactive(input$mmc_feats_from_mod), 1000)
  mmc_feats_from_dim_d <- debounce(reactive(input$mmc_feats_from_dim), 1000)
  mmc_feats_thresh_d <- debounce(reactive(input$mmc_feats_thresh), 1000)

  ###################################################################################################
  # DATA

  data <- reactive({
    if (is.null(input$rdata)) {
      rdata <- input$results
      # name <- rdata
    } else {
      rdata <- input$rdata$datapath
      # name <- input$rdata$name
    }

    ans <- loadData(rdata)

    first_factor_init <- list(ans$ffactors, ans$ffactors[1])
    second_factor_init <- list(ans$ffactors, ans$ffactors[2])
    dataDrivenUIUpdate(session,
      mode=list(ans$modes, ans$modes),
      division=list(ans$divisions, NULL),
      fx=first_factor_init,
      fy=second_factor_init,
      showfactors=list(ans$lfactors, ans$lfactors),
      dimA=first_factor_init,
      feat_crit_dim=first_factor_init,
      cluster_2D_x=first_factor_init,
      cluster_2D_y=second_factor_init,
      cluster_means_dims=list(ans$ffactors, ans$ffactors[1:2])
    )

    ans
  })

  cmpData <- reactive({
    ans <- loadData(input$cmp_results)
    data <- data()
    cmpDataDrivenUIUpdate(session,
      dimB=list(ans$ffactors, ans$ffactors[1])
    )
    ans
  })

  multiData <- reactivePoll(5000, session,
    checkFunc=lsResults,
    valueFunc=function() {
      results <- lsResults()
      models <- vector("list", length(results))
      names(models) <- names(results)
      for (i in 1:length(results)) {
        model_path <- results[i]
        models[[i]] <- loadData(model_path)$ldf
      }
      models
    }
  )

  ###################################################################################################
  # BOOKMARKING

  # NOTE (#DTState 1/3): stateSave must be enabled in the DataTable options in order for the
  # corresponding input storing the state to be populated, but we don't actually want DT state to be
  # restored whenever the same user reopens the app (we just want to save the state for bookmarking
  # purposes), so by default, we override the callback restoring the state so that the stored state
  # is ignored
  session$userData$ltable_state <- ltable_state_default
  onRestore(function(state) {
    # NOTE (#DTState 2/3): only when restoring state from a bookmarked session do we allow the saved
    # state to be applied to the DataTable
    session$userData$ltable_state <- DT::JS(paste0(
      "function() {\n",
      "return ", toJSON(state$input$ltable_state, auto_unbox=TRUE), ";\n",
      "}"
    ))
  })
  onRestored(function(state) {
    dataDrivenUIUpdate(session,
      mode=list(NULL, state$input$mode),
      division=list(NULL, state$input$division),
      fx=list(NULL, state$input$fx),
      fy=list(NULL, state$input$fy),
      showfactors=list(NULL, state$input$showfactors),
      dimA=list(NULL, state$input$dimA),
      feat_crit_dim=list(NULL, state$input$feat_crit_dim),
      cluster_2D_x=list(NULL, state$input$cluster_2D_x),
      cluster_2D_y=list(NULL, state$input$cluster_2D_y),
      cluster_means_dims=list(NULL, state$input$cluster_means_dims)
    )
    cmpDataDrivenUIUpdate(session,
      dimB=list(NULL, state$input$dimB)
    )
  })

  ###################################################################################################
  # TWO-DIMENSIONAL PLOT

  ranges <- reactiveValues(x=NULL, y=NULL)

  output$fplot <- renderPlot({
    data <- data()
    fdf <- data$fdf
    ffactors <- data$ffactors
    plot_type <- input$distPlotType
    fx <- input$fx
    fy <- input$fy
    mode <- input$mode
    division <- input$division
    # there needs to be at least one value selected among the mode and division checkbox groups,
    # but one of the checkbox groups is allowed to be completely unselected, so that's why they
    # should be checked together: they constitute a single requirement for input
    req(fx, fy, c(mode, division))
    filtered <- filter(fdf, MODE %in% mode | DIVISION %in% division)
    if (plot_type %in% c("scatter", "ellipse", "density2d")) {
      plot <- ggplot(filtered, aes_string(fx, fy, color="DIVISION")) +
        geom_point(aes_string(fx, fy), transform(fdf, MODE=NULL), color="grey", alpha=.2) +
          scale_shape_manual(drop=FALSE, values=shapes)
      if (plot_type == "scatter") {
        plot <- plot + geom_point(aes_string(shape="MODE"), alpha=.4, size=5)
      } else if (plot_type == "ellipse") {
        plot <- plot + stat_ellipse()
      } else if (plot_type == "density2d") {
        plot <- plot + stat_density2d()
      }
      # coord_fixed seems to break location reporting for click interaction...?
      plot <- plot + coord_cartesian(xlim=ranges$x, ylim=ranges$y)
    } else {
      gathered <- gather(filtered, "FACTOR", "SCORE", ffactors) %>%
        filter(FACTOR %in% c(fx, fy))
      if (plot_type == "density") {
        plot <- ggplot(gathered, aes(SCORE, color=DIVISION, linetype=MODE)) +
          geom_density()
      } else if (plot_type == "violin") {
        plot <- ggplot(gathered, aes(DIVISION, SCORE, fill=DIVISION, linetype=MODE)) +
          geom_violin(alpha=.5)
      } else if (plot_type == "box") {
        plot <- ggplot(gathered, aes(DIVISION, SCORE, fill=DIVISION, linetype=MODE)) +
          geom_boxplot(alpha=.5)
      }
      plot <- plot + facet_wrap(~FACTOR, ncol=1) +
        scale_linetype_manual(values=lines, drop=FALSE)
    }
    plot + scale_color_manual(values=palette, drop=FALSE) + scale_fill_manual(values=palette, drop=FALSE)
  })

  # When a double-click happens, check if there's a brush on the plot.
  # If so, zoom to the brush bounds; if not, reset the zoom.
  observeEvent(input$fplot_dblclick, {
    brush <- input$fplot_brush
    if (!is.null(brush)) {
      ranges$x <- c(brush$xmin, brush$xmax)
      ranges$y <- c(brush$ymin, brush$ymax)

    } else {
      ranges$x <- NULL
      ranges$y <- NULL
    }
  })

  output$click_info <- renderPrint({
    fplot_click <- input$fplot_click
    fx <- input$fx
    fy <- input$fy
    req(fplot_click, fx, fy)
    # Because it's a ggplot2, we don't need to supply xvar or yvar; if this
    # were a base graphics plot, we'd need those.
    point <- nearPoints(data()$fdf, fplot_click, addDist=TRUE)[1, ]
    id <- point$X
    updateTextInput(session, "top_feats_per_dim_chunk_id", value=id)
    withTags(div(p(b(paste0(fx, ":")), point[[fx]]),
                 p(b(paste0(fy, ":")), point[[fy]]),
                 p(b("MODE:"), point$MODE),
                 p(b("DIVISION:"), point$DIVISION),
                 p(b("SUPERCLASS:"), point$SUPERCLASS),
                 p(b("CLASS:"), point$CLASS),
                 p(b("ID:"), span(id, id="chunk_id"))))
  })

  ###################################################################################################
  # BIBER PLOTS

  dplot <- function(factor) {
    renderPlot({
      factor <- input[[factor]]
      perc <- input$perc
      req(factor, perc)
      DimDraw(data()$fdf, factor.name=factor, low.perc=perc[1], up.perc=perc[2])
    })
  }

  output$d1plot <- dplot("fx")
  output$d2plot <- dplot("fy")

  ###################################################################################################
  # LOADINGS

  ltable <- reactive({
    data <- data()
    thresh <- input$thresh
    showfactors <- input$showfactors
    req(thresh, showfactors)
    # TODO: remove this if we ever fix all data sets to actually include
    # the orig data frames
    ldf <- if (is.null(data$orig)) {
      showNotification(
        "Can't compute correlated features, this data set does not contain the original data frame with feature values.",
        type="warning",
        duration=NULL
      )
      data$ldf
    } else {
      add_correlated_feats(data$ldf, data$orig, input$cor_feat_thresh)
    }
    ldf$Loading <- round(ldf$Loading, 2)
    filter(ldf, (Loading <= thresh[1] | Loading >= thresh[2]) & Factor %in% showfactors) %>%
      spread(Factor, Loading) %>%
      right_join(feat2desc, .)
  })
  output$ltable <- DT::renderDataTable({
    dt <- DT::datatable(
                ltable(), filter="top", style="bootstrap", class=c("compact", "hover"),
                rownames=FALSE, selection="none",
                options=list(
                  stateSave=TRUE,
                  stateLoadCallback=session$userData$ltable_state,
                  paging=FALSE,
                  bInfo=FALSE,
                  select=FALSE,
                  rowCallback=ltable_js
                ))
    # NOTE (#DTState 3/3): once a saved state from a bookmark has been used to restore the
    # datatable, discard it, or else it'll come back to haunt the user (= get re-applied whenever
    # the datatable generation code is re-run)
    session$userData$ltable_state <- ltable_state_default
    dt
  })

  ###################################################################################################
  # GENREDIFF

  genreDiffVals <- reactiveValues(subcorp1=NULL, subcorp2=NULL)

  # globalMeta only contains Koditex chunks and their metadata; this adds any
  # additional chunks and their MODE and DIVISION metadata found in the current
  # results
  globalAndResultsMeta <- reactive({
    fdf <- data()$fdf
    non_koditex_ids <- setdiff(rownames(fdf), globalMeta$id)
    non_koditex_chunks <- fdf[non_koditex_ids, ] %>%
      rownames_to_column("id") %>%
      select(id, mode=MODE, division=DIVISION_ORIG) %>%
      # consider all additional chunks extracted from results as include=yes,
      # otherwise the user would have to change this manually every time, because
      # we want to keep force_include=TRUE for Koditex data
      mutate(include="yes")
    bind_rows(globalMeta, non_koditex_chunks) %>%
      replace(is.na(.), "-NA-")
  })

  subcorpModal <- function(subcorp, meta, selected=list(), force_include=FALSE) {
    if (force_include) selected$include = "yes"
    categories <- colnames(meta) %>%
      purrr::discard(~ .x == "id") %>%
      purrr::map(function(colname) {
        tabPanel(
          colname,
          checkboxGroupInput(
            paste0(colname, "CheckboxSubcorp", subcorp), "",
            choices=sort(unique(meta[[colname]])),
            selected=selected[[colname]]
          )
        )
      })
    modalDialog(
      title=paste("Subcorpus", subcorp),
      size="l",
      span("Specify subcorpus using metadata categories:"),
      br(),
      br(),
      do.call(tabsetPanel, categories),
      footer = tagList(
        modalButton("Cancel"),
        actionButton(paste0("refineSubcorpSelection", subcorp), "Refine Selection"),
        actionButton(paste0("okSubcorp", subcorp), "OK")
      )
    )
  }

  doSubcorpSelection <- function(subcorp) {
    meta <- globalAndResultsMeta()
    chboxes <- grep(paste0("CheckboxSubcorp", subcorp), names(input), value=TRUE)
    selectedList <- list()
    for (chbox in chboxes) {
      selected <- input[[chbox]]
      if (!is.null(selected)) {
        colname <- gsub(paste0("CheckboxSubcorp", subcorp), "", chbox)
        meta <- meta[meta[[colname]] %in% selected, ]
        selectedList[[colname]] <- selected
      }
    }
    list(meta=meta, selected=selectedList)
  }

  refineSubcorpModal <- function(subcorp) {
    refined <- doSubcorpSelection(subcorp)
    removeModal()
    subcorpModal(subcorp, refined$meta, refined$selected)
  }

  confirmSubcorp <- function(subcorp) {
    confirmed <- doSubcorpSelection(subcorp)
    selected <- confirmed$selected
    meta <- confirmed$meta
    catList <- lapply(names(selected), function(n) {
      tagList(tags$li(
        tags$b(paste0(n, ": ", collapse="")), paste(selected[[n]], collapse=", ")))
    })
    catList <- do.call(tags$ul, catList)
    output[[paste0("descSubcorp", subcorp)]] <- renderUI(catList)
    genreDiffVals[[paste0("subcorp", subcorp)]] <- meta$id
    removeModal()
  }

  observeEvent(input$subcorp1, showModal(subcorpModal(1, globalAndResultsMeta(), force_include=TRUE)))
  observeEvent(input$subcorp2, showModal(subcorpModal(2, globalAndResultsMeta(), force_include=TRUE)))
  observeEvent(input$refineSubcorpSelection1, showModal(refineSubcorpModal(1)))
  observeEvent(input$refineSubcorpSelection2, showModal(refineSubcorpModal(2)))
  observeEvent(input$okSubcorp1, confirmSubcorp(1))
  observeEvent(input$okSubcorp2, confirmSubcorp(2))
  observe({
    subcorp1 <- genreDiffVals$subcorp1
    subcorp2 <- genreDiffVals$subcorp2
    output$genreDiffPlot <- renderPlot({
      shiny::validate(
        need(subcorp1, "Please specify subcorpus 1!"),
        need(subcorp2, "Please specify subcorpus 2!")
      )
      genreDiff(data()$fdf, subcorp1, subcorp2)
    })
  })

  ###################################################################################################
  # MODEL COMPARISON

  output$modelCmpPlot <- renderPlot({
    mname1 <- tools::file_path_sans_ext(basename(input$results))
    mname2 <- tools::file_path_sans_ext(basename(input$cmp_results))
    cmpFeats <- modelCmpFeatures(data()$ldf, cmpData()$ldf)
    cmpChunks <- modelCmpChunks(data()$fdf, cmpData()$fdf)
    plotModelCmp(bind_rows(cmpFeats, cmpChunks), mname1, mname2)
  })

  ###################################################################################################
  # LOADINGS COMPARISON

  loadingsCmpReactive <- reactive({
    feat2desc <- select(feat2desc, Feature, Description)
    compare_loadings(
      data()$ldf,
      cmpData()$ldf,
      input$dimA,
      input$dimB,
      feat2desc,
      input$lc_feats_thresh,
      input$lc_only_feats_in_both
    )
  })

  output$loadingsCmpBoth <- renderTable({
    loadingsCmpReactive()$both
  })

  output$loadingsCmpA <- renderTable({
    loadingsCmpReactive()$onlyA
  })

  output$loadingsCmpB <- renderTable({
    loadingsCmpReactive()$onlyB
  })

  ###################################################################################################
  # MULTI-MODEL COMPARISON

  multiModelCmpReactive <- reactive({
    model_seq <- model_seq_d()
    shiny::validate(
      need(
        length(model_seq) > 1,
        "Please specify at least two models in the sequence!"
      )
    )
    multi_data <- multiData()
    feats_from_mod <- mmc_feats_from_mod_d()
    feats_from_dim <- mmc_feats_from_dim_d()
    feat_set <- if (!is.null(feats_from_mod) && !is.null(feats_from_dim)) {
      feats_thresh <- mmc_feats_thresh_d()
      tmp <- bind_rows(multi_data[feats_from_mod]) %>%
        filter(
          grepl(sprintf("%s$", feats_from_dim), Factor) &
            (Loading <= feats_thresh[1] | Loading >= feats_thresh[2])
        )
      unique(c(as.character(tmp$Feature), mmc_feat_set_d()))
    } else {
      mmc_feat_set_d()
    }
    models <- multi_data[model_seq]
    res <- multiModelCmp(models, featSet=feat_set)
    list(mmc=res$models, feat_set=length(feat_set) > 0, details=res$details)
  })

  output$multiModelCmpPlot <- renderPlot({
    mmc <- multiModelCmpReactive()
    plotMultiModelCmp(mmc$mmc, featSet=mmc$feat_set)
  })

  output$multiModelCmpDetails <- renderTable({
    details <- multiModelCmpReactive()$details
    m1 <- input$mmc_dets_m1
    m2 <- input$mmc_dets_m2
    if (nrow(details) > 0 && m1 %in% details$ModelName1 && m2 %in% details$ModelName2) {
      desc <- select(feat2desc, Feature, Description)
      details <- filter(details, ModelName1 == m1 & ModelName2 == m2) %>%
        top_n(input$mmc_dets_top, abs(LoadProd)) %>%
        arrange(Feature) %>%
        spread(Factors, LoadProd) %>%
        select(-ModelName1, -ModelName2) %>%
        left_join(desc) %>%
        select(Feature, Description, everything())
    } else {
      details <- tibble()
    }
    details
  })

  ###################################################################################################
  # FEATURE CRITIC

  output$featCritPlot <- renderPlot({
    plotFeatCrit(data()$feat_crit_table, input$feat_crit_dim)
  })

  ###################################################################################################
  # TOP FEATURES PER DIMENSION

  top_feats_per_dim_thresh_d <- debounce(reactive(input$top_feats_per_dim_thresh), 1000)
  top_feats_per_dim_chunk_id_d <- debounce(reactive(input$top_feats_per_dim_chunk_id), 1000)
  top_feats_per_dim_meta_regex_d <- debounce(reactive(input$top_feats_per_dim_meta_regex), 1000)

  topFeatsPerDimReactive <- reactive({
    data <- data()
    # TODO: remove this if we ever fix all data sets to actually include
    # the orig data frames (based on which the norm data frame is
    # computed)
    shiny::validate(
      need(data$norm, "Can't show top features, this data set does not contain the original data frame with feature values.")
    )
    top_feature_boxplot(
      data$norm,
      data$ldf,
      feat2desc,
      input$feat_crit_dim,
      top_feats_per_dim_thresh_d(),
      top_feats_per_dim_chunk_id_d(),
      top_feats_per_dim_meta_regex_d()
    )
  })

  output$topFeatsPerDimPlot <- renderPlot(
    topFeatsPerDimReactive()$plot,
    height=function() {
      300*topFeatsPerDimReactive()$nrow
  })

  ###################################################################################################
  # TEXT TYPES

  factorsClustersReactive <- reactive({
    add_clusters(data()$fdf, input$cluster_k)
  })
  output$cluster2DPlot <- renderPlot(
    cluster_2D_plot(factorsClustersReactive(), dim_x=input$cluster_2D_x, dim_y=input$cluster_2D_y, a=input$cluster_2D_a)
  )
  output$clusterMeansPlot <- renderPlot(
    cluster_means_plot(factorsClustersReactive(), dims=input$cluster_means_dims)
  )
  output$clusterInfo <- renderTable(
    cluster_info(factorsClustersReactive(), cluster=input$cluster_info_cluster, topn=input$cluster_info_topn)
  )
  output$clusterSizes <- renderTable(
    cluster_sizes(factorsClustersReactive())
  )
}
