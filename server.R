library(ggplot2)
library(Cairo)   # For nicer ggplot2 output when deployed on Linux

palette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

defaultIfEmptyString <- function(value, default) {
  if (value == "") default else value
}

shinyServer(function(input, output, session) {
  data <- reactive({
    if (is.null(input$csv)) {
      csv <- "factors.csv"
      # name <- csv
    } else {
      csv <- input$csv$datapath
      # name <- input$csv$name
    }

    df <- read.csv(csv)
    factors <- grep("^(X|MODE|DIVISION|SUPERCLASS|CLASS)$", colnames(df), value=TRUE, invert=TRUE)
    df$DIVISION <- factor(df$DIVISION, c("int", "nin", "mul", "uni", "fic", "nfc", "nmg", "pri"),
                          c("spo int", "spo nin", "web mul", "web uni", "wri fic", "wri nfc", "wri nmg", "wri pri"))
    updateSelectInput(session, "fx", choices=factors, selected=factors[1])
    updateSelectInput(session, "fy", choices=factors, selected=factors[2])
    modes <- levels(df$MODE)
    updateCheckboxGroupInput(session, "mode", choices=modes, selected=modes, inline=TRUE)
    divisions <- levels(df$DIVISION)
    updateCheckboxGroupInput(session, "division", choices=divisions, inline=TRUE)

    list(df=df, factors=factors, modes=modes, divisions=divisions)
  })

  ranges <- reactiveValues(x=NULL, y=NULL)

  output$mdaplot <- renderPlot({
    data <- data()
    df <- data$df
    factors <- data$factors
    fx <- defaultIfEmptyString(input$fx, factors[1])
    fy <- defaultIfEmptyString(input$fy, factors[2])
    filtered <- subset(df, MODE %in% input$mode | DIVISION %in% input$division)
    ggplot(filtered, aes_string(fx, fy, color="DIVISION")) +
      geom_point(aes_string(fx, fy), transform(df, MODE=NULL), color="grey", alpha=.2) +
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
    data <- data()
    # Because it's a ggplot2, we don't need to supply xvar or yvar; if this
    # were a base graphics plot, we'd need those.
    point <- nearPoints(data$df, input$mdaplot_click, addDist=TRUE)[1, ]
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

  output$fx <- reactive(input$fx)
  output$fy <- reactive(input$fy)
})
