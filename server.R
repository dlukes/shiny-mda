library(ggplot2)
library(purrr)
library(tidyr)
library(dplyr)
library(readr)
library(jsonlite)
library(Cairo)  # For nicer ggplot2 output when deployed on Linux

source("dim_graph.R")
source("genrediff.R")
source("filterRange_override.R")

palette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
theme_set(theme_bw(base_size=18))
feat2desc <- read.csv("./conf/feat2desc.csv")
globalMeta <- read_delim("./conf/koditex-metadata.csv", delim="\t")
ltable_js <- DT::JS(read_file("./www/loadingsTable.js"))
ltable_state_default <- DT::JS("function(settings, data) { return false; }")

# This function encapsulates all changes to the UI that are driven by input data, which means they
# need a special treatment w.r.t. bookmarking.
dataDrivenUIUpdate <- function(session, mode, division, fx, fy, showfactors) {
  updateCheckboxGroupInput(session, "mode", choices=mode[[1]], selected=mode[[2]])
  updateCheckboxGroupInput(session, "division", choices=division[[1]], selected=division[[2]])
  updateSelectInput(session, "fx", choices=fx[[1]], selected=fx[[2]])
  updateSelectInput(session, "fy", choices=fy[[1]], selected=fy[[2]])
  updateCheckboxGroupInput(session, "showfactors", choices=showfactors[[1]], selected=showfactors[[2]])
}

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
    modes <- levels(fdf$MODE)
    divisions <- levels(fdf$DIVISION)

    ldf <- unclass(env$load)
    ldf <- data.frame(Feature=row.names(ldf), ldf) %>%
      gather(Factor, Loading, -Feature, factor_key=TRUE)
    lfactors <- levels(ldf$Factor)

    dataDrivenUIUpdate(session,
      mode=list(modes, modes),
      division=list(divisions, NULL),
      fx=list(ffactors, ffactors[1]),
      fy=list(ffactors, ffactors[2]),
      showfactors=list(lfactors, lfactors)
    )

    list(fdf=fdf, ldf=ldf, lfactors=lfactors)
  })

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
      showfactors=list(NULL, state$input$showfactors)
    )
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

  subcorpModal <- function(subcorp, meta=globalMeta, selected=list()) {
    categories <- colnames(meta) %>%
      discard(~ .x == "id") %>%
      map(function(colname) {
        tabPanel(
          colname,
          checkboxGroupInput(paste0(colname, "CheckboxSubcorp", subcorp), "",
                             choices=sort(unique(meta[[colname]])),
                             selected=selected[[colname]])
        )
      })
    modalDialog(
      title=paste("Subcorpus", subcorp),
      size="l",
      span("Specify subcorpus using metadata categories:"),
      do.call(tabsetPanel, categories),
      footer = tagList(
        modalButton("Cancel"),
        actionButton(paste0("refineSubcorpSelection", subcorp), "Refine Selection"),
        actionButton(paste0("okSubcorp", subcorp), "OK")
      )
    )
  }

  doSubcorpSelection <- function(subcorp) {
    meta <- globalMeta
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
    subcorpModal(1, refined$meta, refined$selectedList)
  }

  confirmSubcorp <- function(subcorp) {
    confirmed <- doSubcorpSelection(subcorp)
    selected <- confirmed$selected
    meta <- confirmed$meta
    catList <- lapply(names(selected), function(n) {
      tagList(tags$li(
        tags$b(paste(n, ": ", collapse="")), paste(selected[[n]], collapse=", ")))
    })
    catList <- do.call(tags$ul, catList)
    output[[paste0("descSubcorp", subcorp)]] <- renderUI(catList)
    genreDiffVals[[paste0("subcorp", subcorp)]] <- meta$id
    removeModal()
  }

  observeEvent(input$subcorp1, showModal(subcorpModal(1)))
  observeEvent(input$subcorp2, showModal(subcorpModal(2)))
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
}
