fresults <- sort(list.files("results", pattern="\\.RData$", full.names=TRUE), decreasing=TRUE)
names(fresults) <- tools::file_path_sans_ext(basename(fresults))

function(request) { fluidPage(
  titlePanel("Shiny Happy MDA"),
  tags$head(tags$link(rel="stylesheet", type="text/css", href="mda.css")),

  sidebarLayout(
    sidebarPanel(
      width=3,
      bookmarkButton(),
      fileInput("rdata", "Upload your own data:", accept=".RData"),
      selectInput("results", "Results:", choices=fresults, selected=fresults[1]),
      conditionalPanel(condition="/^Factors/.test(input.tabsetPanel)",
        selectInput("fx", "X axis:", choices=c()),
        selectInput("fy", "Y axis:", choices=c())
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
        )
      )
    )
  ),
  tags$script(src="kontextSearch.js")
)}
