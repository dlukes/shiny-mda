library(ggplot2)
library(tidyr)
library(dplyr)
library(readr)
library(Cairo)  # For nicer ggplot2 output when deployed on Linux

source("dim_graph.R")

palette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
feat2desc <- read.csv("./conf/feat2desc.csv")
ltable_js <- read_file("./www/loadingsTable.js")

function(input, output, session) {
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

    list(fdf=fdf, ldf=ldf, lfactors=lfactors)
  })

  ###################################################################################################
  # TWO-DIMENSIONAL PLOT

  ranges <- reactiveValues(x=NULL, y=NULL)

  output$fplot <- renderPlot({
    fdf <- data()$fdf
    fx <- input$fx
    fy <- input$fy
    mode <- input$mode
    division <- input$division
    # there needs to be at least one value selected among the mode and division checkbox groups,
    # but one of the checkbox groups is allowed to be completely unselected, so that's why they
    # should be checked together: they constitute a single requirement for input
    req(fx, fy, c(mode, division))
    filtered <- filter(fdf, MODE %in% mode | DIVISION %in% division)
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
    fplot_click <- input$fplot_click
    fx <- input$fx
    fy <- input$fy
    req(fplot_click, fx, fy)
    # Because it's a ggplot2, we don't need to supply xvar or yvar; if this
    # were a base graphics plot, we'd need those.
    point <- nearPoints(data()$fdf, fplot_click, addDist=TRUE)[1, ]
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
    showfactors <- input$showfactors
    req(thresh, showfactors)
    filter(data$ldf, (Loading < thresh[1] | Loading > thresh[2]) & Factor %in% showfactors) %>%
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
}
