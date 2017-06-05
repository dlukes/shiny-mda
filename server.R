library(ggplot2)
library(tidyr)
library(dplyr)
library(Cairo)   # For nicer ggplot2 output when deployed on Linux

source("dim_graph.R")

palette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

defaultIfEmptyString <- function(value, default) {
  if (value == "") default else value
}

shinyServer(function(input, output, session) {
  data <- reactive({
    if (is.null(input$csv)) {
      csv <- input$results
      # name <- csv
    } else {
      csv <- input$csv$datapath
      # name <- input$csv$name
    }

    fdf <- read.csv(csv)
    factors <- grep("^(X|MODE|DIVISION|SUPERCLASS|CLASS)$", colnames(fdf), value=TRUE, invert=TRUE)
    fdf$DIVISION <- factor(fdf$DIVISION, c("int", "nin", "mul", "uni", "fic", "nfc", "nmg", "pri"),
                          c("spo-int", "spo-nin", "web-mul", "web-uni", "wri-fic", "wri-nfc", "wri-nmg", "wri-pri"))

    updateSelectInput(session, "fx", choices=factors, selected=factors[1])
    updateSelectInput(session, "fy", choices=factors, selected=factors[2])
    modes <- levels(fdf$MODE)
    updateCheckboxGroupInput(session, "mode", choices=modes, selected=modes, inline=TRUE)
    divisions <- levels(fdf$DIVISION)
    updateCheckboxGroupInput(session, "division", choices=divisions, inline=TRUE)

    list(fdf=fdf, factors=factors, modes=modes, divisions=divisions)
  })

  ranges <- reactiveValues(x=NULL, y=NULL)

  output$fplot <- renderPlot({
    data <- data()
    fdf <- data$fdf
    factors <- data$factors
    fx <- defaultIfEmptyString(input$fx, factors[1])
    fy <- defaultIfEmptyString(input$fy, factors[2])
    filtered <- subset(fdf, MODE %in% input$mode | DIVISION %in% input$division)
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
    data <- data()
    # Because it's a ggplot2, we don't need to supply xvar or yvar; if this
    # were a base graphics plot, we'd need those.
    point <- nearPoints(data$fdf, input$fplot_click, addDist=TRUE)[1, ]
    factors <- data$factors
    fx <- defaultIfEmptyString(input$fx, factors[1])
    fy <- defaultIfEmptyString(input$fy, factors[2])
    id <- point$X
    withTags(div(p(b(paste0(fx, ":")), point[[fx]]),
                 p(b(paste0(fy, ":")), point[[fy]]),
                 p(b("MODE:"), point$MODE),
                 p(b("DIVISION:"), point$DIVISION),
                 p(b("SUPERCLASS:"), point$SUPERCLASS),
                 p(b("CLASS:"), point$CLASS),
                 p(b("ID:"), span(id, id="chunk_id"))))
  })

  output$lplot <- renderPlot({
    if (!is.null(input$csv)) {
      stop("Loadings are available only with preloaded data.")
    }

    ldf <- read.csv(file.path("results", "loadings", basename(input$results)))
    ggplot(ldf, aes(Feature, abs(Loading), fill=Loading)) +
      facet_wrap(~Factor, nrow=1) +
      geom_bar(stat="identity") +
      coord_flip() +
      scale_fill_gradient2(name="Loading",
                           high="blue", mid="white", low="red",
                           midpoint=0, guide="colourbar") +
      ylab("Loading Strength") +
      theme_bw(base_size=8)
  })

  output$d1plot <- renderPlot({
    data <- data()
    DimDraw(data$fdf, factor.name=input$fx, low.perc=input$range[1], up.perc=input$range[2])
  })
  output$d2plot <- renderPlot({
    data <- data()
    DimDraw(data$fdf, factor.name=input$fy, low.perc=input$range[1], up.perc=input$range[2])
  })
})
