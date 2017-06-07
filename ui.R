fresults <- sort(list.files("results", pattern="\\.RData$", full.names=TRUE), decreasing=TRUE)
names(fresults) <- tools::file_path_sans_ext(basename(fresults))

shinyUI(fluidPage(
  titlePanel("Shiny Happy MDA"),
  tags$head(tags$link(rel="stylesheet", type="text/css", href="mda.css")),

  sidebarLayout(
    sidebarPanel(
      width=3,
      # bookmarking probably requires some more setup, or maybe it fails because of the initial error
      # in the app...?
      # bookmarkButton(),
      fileInput("rdata", "Upload your own data:", accept=".RData"),
      selectInput("results", "Results:", choices=fresults, selected=fresults[1]),
      conditionalPanel(condition="input.tabsetPanel == 'Factors 2-dim'",
        selectInput("fx", "X axis:", choices=c()),
        selectInput("fy", "Y axis:", choices=c()),
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
          sliderInput("thresh", "Thresholds:", min=-1, max=1, step=.05, value=c(-.3, .3), width="100%")
        ),
        checkboxGroupInput("showfactors", "Show factors:", choices=c(), width="100%"),
        selectInput("sortfactor", "Sort by:", choices=c())
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
          tableOutput("ltable")
        )
      )
    )
  ),
  tags$script(src="kontextSearch.js"),
  tags$script(src="loadingsTable.js")
))
