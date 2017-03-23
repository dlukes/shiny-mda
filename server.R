library(dplyr)
library(ggplot2)
library(Cairo)   # For nicer ggplot2 output when deployed on Linux

palette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

shinyServer(function(input, output, session) {
  data <- reactive({
    if (is.null(input$csv)) {
      csv <- "factors.csv"
      name <- csv
    } else {
      csv <- input$csv$datapath
      # name <- input$csv$name
    }
    
    factors <- read.csv(csv)
    fnames <- grep("^(X|MODE|DIVISION|SUPERCLASS|CLASS)$", colnames(factors), value=TRUE, invert=TRUE)
    factors <- rename(factors, DIVISION_ORIG=DIVISION)
    factors$DIVISION <- factor(paste(factors$MODE, factors$DIVISION_ORIG))
    updateSelectInput(session, "fx", choices=fnames)
    updateSelectInput(session, "fy", choices=rev(fnames))
    modes <- levels(factors$MODE)
    updateCheckboxGroupInput(session, "mode", choices=modes, selected=modes, inline=TRUE)
    divisions <- levels(factors$DIVISION_ORIG)
    updateCheckboxGroupInput(session, "division", choices=divisions, inline=TRUE)

    list(factors=factors)
  })

  ranges <- reactiveValues(x=NULL, y=NULL)
  
  output$mdaplot <- renderPlot({
    fx <- input$fx
    fy <- input$fy
    factors <- data()$factors
    filtered <- subset(factors, MODE %in% input$mode | DIVISION_ORIG %in% input$division)
    ggplot(filtered, aes_string(fx, fy, color="DIVISION")) +
      geom_point(aes_string(fx, fy), transform(factors, MODE=NULL), color="grey", alpha=.2) +
      geom_point(aes_string(shape="MODE"), alpha=.4, size=5) +
      theme_bw() +
      scale_color_manual(values=palette, drop=FALSE) +
      scale_shape_discrete(drop=FALSE) +
      # coord_fixed seems to break location reporting for click interaction...?
      coord_cartesian(xlim=ranges$x, ylim=ranges$y)
  })
  
  # When a double-click happens, check if there's a brush on the plot.
  # If so, zoom to the brush bounds; if not, reset the zoom.
  observeEvent(input$mdaplot_dblclick, {
    brush <- input$mdaplot_brush
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
    point <- nearPoints(data()$factors, input$mdaplot_click, addDist=TRUE)[1, ]
    kfx <- input$fx
    kfy <- input$fy
    id <- point[, "X"]
    query <- paste0("q=", URLencode(paste0('q[] within <chunk id="', if (is.na(id)) ".*" else id, '"/>'), reserved=TRUE))
    href <- paste0("https://kontext.korpus.cz/view?corpname=koditex&default_attr=word&", query)
    withTags(div(p(b(paste0(kfx, ":")), point[kfx]),
                 p(b(paste0(kfy, ":")), point[kfy]),
                 p(b("MODE:"), point$MODE),
                 p(b("DIVISION:"), point$DIVISION_ORIG),
                 p(b("SUPERCLASS:"), point$SUPERCLASS),
                 p(b("CLASS:"), point$CLASS),
                 p(b("ID:"), span(id, id="chunk_id"))))
  })
  
  output$fx <- reactive(input$fx)
  output$fy <- reactive(input$fy)
})
