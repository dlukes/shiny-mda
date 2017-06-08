library(ggplot2)
library(tidyr)
library(dplyr)
library(readr)
library(Cairo)  # For nicer ggplot2 output when deployed on Linux

source("dim_graph.R")

palette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
feat2desc <- read.csv("./conf/feat2desc.csv")
ltable_js <- read_file("./www/loadingsTable.js")

shinyServer(function(input, output, session) {
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

    env <- new.env()
    load(rdata, env)
    env <- as.list(env)

    fdf <- env$factors
    fdf$X <- row.names(fdf)
    ffactors <- grep("^(X|MODE|DIVISION|SUPERCLASS|CLASS)$", colnames(fdf), value=TRUE, invert=TRUE)
    fdf$DIVISION <- factor(fdf$DIVISION, c("int", "nin", "mul", "uni", "fic", "nfc", "nmg", "pri"),
                          c("spo-int", "spo-nin", "web-mul", "web-uni", "wri-fic", "wri-nfc", "wri-nmg", "wri-pri"))
    updateSelectInput(session, "fx", choices=ffactors, selected=ffactors[1])
    updateSelectInput(session, "fy", choices=ffactors, selected=ffactors[2])
    modes <- levels(fdf$MODE)
    updateCheckboxGroupInput(session, "mode", choices=modes, selected=modes)
    divisions <- levels(fdf$DIVISION)
    updateCheckboxGroupInput(session, "division", choices=divisions)

    ldf <- unclass(env$load)
    ldf <- data.frame(Feature=row.names(ldf), ldf) %>%
      gather(Factor, Loading, -Feature, factor_key=TRUE)
    lfactors <- levels(ldf$Factor)
    updateCheckboxGroupInput(session, "showfactors", choices=lfactors, selected=lfactors)
    updateSelectInput(session, "sortfactor", choices=c(colnames(feat2desc), lfactors))

    list(fdf=fdf, ldf=ldf, lfactors=lfactors)
  })

  ###################################################################################################
  # TWO-DIMENSIONAL PLOT

  ranges <- reactiveValues(x=NULL, y=NULL)

  output$fplot <- renderPlot({
    fdf <- data()$fdf
    fx <- input$fx
    fy <- input$fy
    filtered <- filter(fdf, MODE %in% input$mode | DIVISION %in% input$division)
    # NOTE: For some reason, the inputs may not be completely initialized when the first attempt to
    # render the plot is made, in which case the filtering above returns an empty data frame and
    # plotting it would result in an error. Returning early when the data frame is empty prevents
    # the error from happening (= no ugly red message shown to the user) and the plot is re-rendered
    # as soon as the inputs are initialized.
    #
    # In theory, all rendering calls might be affected by this, but in practice, since this is the
    # tab shown by default in the UI, it's sufficient to perform the check only here.
    #
    # An alternative / previous solution was to return possible default values as part of the `data`
    # reactive and use a `getDefault()` function to revert to a fallback when the value was
    # unspecified, but that actually leads to nasty corner cases, because inputs may be empty both
    # because they're uninitialized OR because the user has unchecked all checkboxes (for instance),
    # in which case we actually want to keep the empty value. Handling all of this properly is more
    # trouble than it's worth, exiting on an empty source data frame is simpler and cleaner.
    if (nrow(filtered) == 0) {
      return()
    }
    ggplot(filtered, aes_string(fx, fy, color="DIVISION")) +
      geom_point(aes_string(fx, fy), transform(fdf, MODE=NULL), color="grey", alpha=.2) +
      geom_point(aes_string(shape="MODE"), alpha=.4, size=5) +
      theme_bw() +
      scale_color_manual(values=palette, drop=FALSE) +
      scale_shape_discrete(drop=FALSE) +
      # coord_fixed seems to break location reporting for click interaction...?
      coord_cartesian(xlim=ranges$x, ylim=ranges$y)
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
    # Because it's a ggplot2, we don't need to supply xvar or yvar; if this
    # were a base graphics plot, we'd need those.
    point <- nearPoints(data()$fdf, input$fplot_click, addDist=TRUE)[1, ]
    fx <- input$fx
    fy <- input$fy
    id <- point$X
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

  output$d1plot <- renderPlot({
    DimDraw(data()$fdf, factor.name=input$fx, low.perc=input$perc[1], up.perc=input$perc[2])
  })
  output$d2plot <- renderPlot({
    DimDraw(data()$fdf, factor.name=input$fy, low.perc=input$perc[1], up.perc=input$perc[2])
  })

  ###################################################################################################
  # LOADINGS

  output$lplot <- renderPlot({
    ggplot(data()$ldf, aes(Feature, abs(Loading), fill=Loading)) +
      facet_wrap(~Factor, nrow=1) +
      geom_bar(stat="identity") +
      coord_flip() +
      scale_fill_gradient2(name="Loading",
                           high="blue", mid="white", low="red",
                           midpoint=0, guide="colourbar") +
      ylab("Loading Strength") +
      theme_bw(base_size=8)
  })

  ltable <- reactive({
    data <- data()
    thresh <- input$thresh
    filter(data$ldf, (Loading < thresh[1] | Loading > thresh[2]) & Factor %in% input$showfactors) %>%
      spread(Factor, Loading) %>%
      inner_join(feat2desc, .)
  })
  # NOTE: if a reactive function parameter needs to use a reactive value, wrap it in a function,
  # it will be evaluated in an active reactive context
  align <- function() {
    align <- rep("?", ncol(ltable()))
    align[c(1, 2)] <- c("r", "l")
    align <- paste0(align, collapse="")
  }
  output$ltable <- DT::renderDataTable(
    ltable(), filter="top", style="bootstrap", class=c("compact", "hover"), rownames=FALSE, selection="none",
    options=list(
      paging=FALSE,
      bInfo=FALSE,
      searching=FALSE,
      select=FALSE,
      rowCallback=DT::JS(ltable_js)
  ))
})
