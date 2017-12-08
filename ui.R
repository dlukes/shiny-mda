function(request) {
  fresults <- lsResults()
  fresults_names <- names(fresults)
  init_model_seq <- grep("2017-09-18_", fresults_names, value=TRUE)

  fluidPage(
    titlePanel("Shiny Happy MDA"),
    tags$head(tags$link(rel="stylesheet", type="text/css", href="mda.css")),

    sidebarLayout(
      sidebarPanel(
        id="controls-outer",
        width=3,
        div(
          id="controls-inner",
          bookmarkButton(),
          conditionalPanel(condition="input.tabsetPanel != 'MultiModelCmp'",
            fileInput("rdata", "Upload your own data:", accept=".RData"),
            selectInput("results", "Results:", choices=fresults, selected=fresults[1]),
            conditionalPanel(condition="/^Factors/.test(input.tabsetPanel)",
              selectInput("fx", "X axis:", choices=c()),
              selectInput("fy", "Y axis:", choices=c())
            )
          ),
          conditionalPanel(condition="input.tabsetPanel == 'Factors 2-dim'",
            fluidRow(
              column(6, checkboxGroupInput("mode", "Mode:", choices=c())),
              column(6, checkboxGroupInput("division", "Division:", choices=c()))
            ),
            h4("Selected chunk:"),
            htmlOutput("click_info"),
            textInput("cql", "CQL:", value='[lemma=".*"]'),
            actionButton("search", "Search", onclick="kontextSearch()")
          ),
          conditionalPanel(condition="input.tabsetPanel == 'Factors Boxplots'",
            div(
              class="outer-range-wrapper",
              sliderInput("perc", "Percentiles:", min=0, max=1, step=.05, value=c(.3, .7), width="100%")
            )
          ),
          conditionalPanel(condition="input.tabsetPanel == 'Loadings Table'",
            div(
              class="outer-range-wrapper",
              sliderInput("thresh", "Global thresholds:", min=-1, max=1, step=.05, value=c(-.3, .3), width="100%")
            ),
            checkboxGroupInput("showfactors", "Show factors:", choices=c(), width="100%")
          ),
          conditionalPanel(condition="input.tabsetPanel == 'GenreDiff'",
            actionButton("subcorp1", "Specify Subcorpus 1"),
            br(),
            br(),
            actionButton("subcorp2", "Specify Subcorpus 2")
          ),
          conditionalPanel(condition="input.tabsetPanel == 'ModelCmp'",
            selectInput("cmp_results", "Compare with:", choices=fresults, selected=fresults[2])
          ),
          conditionalPanel(condition="input.tabsetPanel == 'MultiModelCmp'",
            selectizeInput("model_seq", "Sequence of models:", fresults_names, selected=init_model_seq, multiple=TRUE),
            h4("Focus on features:"),
            selectizeInput("mmc_feat_set", "Individual:", feat2desc$Feature, multiple=TRUE),
            selectizeInput("mmc_feats_from_mod", "Correlated with model:", choices=fresults_names, multiple=TRUE),
            selectizeInput("mmc_feats_from_dim", "Dimension:", choices=1:9, multiple=TRUE),
            div(
              class="outer-range-wrapper",
              sliderInput("mmc_feats_thresh", "Loading threshold:", min=-1, max=1, step=.05, value=c(-.3, .3), width="100%")
            ),
            h4("Details table (only when focus specified):"),
            selectInput("mmc_dets_m1", "From model:", fresults_names, selected=init_model_seq[1]),
            selectInput("mmc_dets_m2", "To model:", fresults_names, selected=init_model_seq[2]),
            numericInput("mmc_dets_top", "Show only top loading products:", 20, min=0, step=1)
          )
        )
      ),

      mainPanel(
        id="mainPanel",
        width=9,
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
                      )
            )
          ),
          tabPanel(
            "Factors Boxplots",
            plotOutput("d1plot"),
            plotOutput("d2plot")
          ),
          tabPanel(
            "Loadings Plot",
            plotOutput("lplot")
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
            plotOutput("genreDiffPlot")
          ),
          tabPanel(
            "ModelCmp",
            plotOutput("modelCmpPlot")
          ),
          tabPanel(
            "MultiModelCmp",
            plotOutput("multiModelCmpPlot"),
            tableOutput("multiModelCmpDetails")
          )
        )
      )
    ),
    tags$script(src="kontextSearch.js")
  )
}
