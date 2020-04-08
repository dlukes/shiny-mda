help_popover_counter <- 1
help_popover <- function(content, title = NULL) {
  id <- paste0("mdavis-help-popover-", help_popover_counter)
  help_popover_counter <<- help_popover_counter + 1
  tagList(
    bsButton(id, label = "", icon = icon("question"), style = "info", size = "extra-small"),
    bsPopover(id, title = title, content = content, trigger = "click", placement = "right")
  )
}

function(request) {
  bootstrapPage(
  shiny_cnc_UI(),

  fluidPage(
    titlePanel("MDAvis"),
    tags$head(tags$link(rel="stylesheet", type="text/css", href="mda.css")),

    sidebarLayout(
      sidebarPanel(
        id="controls-outer",
        width=2,
        div(
          id="controls-inner",
          conditionalPanel(condition="/^Factors/.test(input.tabsetPanel)",
            selectInput("fx", "X axis:", choices=c()),
            selectInput("fy", "Y axis:", choices=c())
          ),
          conditionalPanel(condition="input.tabsetPanel == 'LoadingsCmp'",
            selectInput("dimA", "Dimension A:", choices=c())
          ),
          conditionalPanel(condition="input.tabsetPanel == 'Factors 2-dim'",
            selectInput("distPlotType", "Plot type:",
                        list(Combined=c("scatter", "ellipse", "density2d"),
                             Separate=c("density", "violin", "box"))),
            fluidRow(
              column(6, checkboxGroupInput("mode", "Mode:", choices=c())),
              column(6, checkboxGroupInput("division", "Division:", choices=c()))
            ),
            h4(
              help_popover(paste(
                "Click on a point to select it and display its metadata.",
                "Click and drag to create a region, double-click on it to zoom in.",
                "Double-click again to zoom back out."
              )),
              "Selected chunk:"
            ),
            htmlOutput("click_info"),
            textInput(
              "cql",
              tagList(
                help_popover(paste(
                   "When a point is selected, enter a CQL query and hit Search",
                   "to open KonText and search for matches in the corresponding",
                   "chunk in the Koditex corpus."
                )),
                "CQL:"
              ),
              value='[lemma=".*"]'
            ),
            actionButton("search", "Search", onclick="kontextSearch()")
          ),
          conditionalPanel(condition="input.tabsetPanel == 'Factors Boxplots'",
            div(
              class="outer-range-wrapper",
              sliderInput(
                "perc",
                tagList(
                  help_popover(paste(
                    "If applicable, a dotted vertical line indicates the split ",
                    "between the categories falling into the left vs. the right ",
                    "range."
                  )),
                  "Percentiles:"
                ),
                min=0, max=1, step=.05, value=c(.3, .7), width="100%"
              )
            )
          ),
          conditionalPanel(condition="input.tabsetPanel == 'Loadings Table'",
            div(
              class="outer-range-wrapper",
              sliderInput("thresh", "Global thresholds:", min=-1, max=1, step=.05, value=c(-.3, .3), width="100%")
            ),
            div(
              class="outer-range-wrapper",
              sliderInput("cor_feat_thresh", "Correlated feature thresholds:", min=-1, max=1, step=.01, value=c(-.95, .95), width="100%")
            ),
            checkboxGroupInput("showfactors", "Show factors:", choices=c(), width="100%")
          ),
          conditionalPanel(condition="input.tabsetPanel == 'GenreDiff'",
            actionButton("subcorp1", "Specify Subcorpus 1"),
            br(),
            br(),
            actionButton("subcorp2", "Specify Subcorpus 2")
          ),
          conditionalPanel(condition="input.tabsetPanel == 'LoadingsCmp'",
            selectInput("dimB", "Dimension B:", choices=c()),
            div(
              class="outer-range-wrapper",
              sliderInput("lc_feats_thresh", "Loading threshold:", min=-1, max=1, step=.05, value=c(-.3, .3), width="100%")
            ),
            checkboxInput("lc_only_feats_in_both", "Only show features present in both models")
          ),
          conditionalPanel(condition="['Feature Critic', 'TopFeatsPerDim'].includes(input.tabsetPanel)",
            selectInput("feat_crit_dim", "Dimension:", choices=c())
          ),
          conditionalPanel(condition="input.tabsetPanel == 'TopFeatsPerDim'",
            div(
              class="outer-range-wrapper",
              sliderInput("top_feats_per_dim_thresh", "Loading threshold:", min=-1, max=1, step=.05, value=c(-.3, .3), width="100%")
            ),
            textInput("top_feats_per_dim_chunk_id", "Chunk ID:"),
            textInput("top_feats_per_dim_meta_regex", "Show classes matching regex:", placeholder="e.g. ^spo-")
          )
        )
      ),

      mainPanel(
        id="mainPanel",
        width=10,
        tabsetPanel(
          id="tabsetPanel",
          tabPanel(
            "Factors 2-dim",
            plotOutput("fplot",
                      click="fplot_click",
                      dblclick="fplot_dblclick",
                      brush=brushOpts(
                        id="fplot_brush",
                        resetOnNew=TRUE
                      ),
                      height="90vh"
            )
          ),
          tabPanel(
            "Factors Boxplots",
            plotOutput("d1plot"),
            plotOutput("d2plot")
          ),
          tabPanel(
            "Loadings Table",
            DT::dataTableOutput("ltable")
          ),
          tabPanel(
            "GenreDiff",
            fluidRow(
              column(6,
                conditionalPanel(condition="output.descSubcorp1",
                  h4("Subcorpus 1")
                ),
                uiOutput("descSubcorp1")
              ),
              column(6,
                conditionalPanel(condition="output.descSubcorp2",
                  h4("Subcorpus 2")
                ),
                uiOutput("descSubcorp2")
              )
            ),
            plotOutput("genreDiffPlot", height="80vh")
          ),
          tabPanel(
              "LoadingsCmp",
              h2("Features above threshold…"),
              h3("… in both dimensions:"),
              tableOutput("loadingsCmpBoth"),
              h3("… only in dimension A:"),
              tableOutput("loadingsCmpA"),
              h3("… only in dimension B:"),
              tableOutput("loadingsCmpB")
          ),
          tabPanel(
            "Feature Critic",
            plotOutput("featCritPlot", height="90vh")
          ),
          tabPanel(
            "TopFeatsPerDim",
            plotOutput("topFeatsPerDimPlot")
          )
        )
      )
    ),
    tags$script(src="kontextSearch.js")
  )
  )
}
