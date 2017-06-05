fresults <- sort(list.files("results/factors", pattern="\\.csv$", full.names=TRUE), decreasing=TRUE)
names(fresults) <- tools::file_path_sans_ext(basename(fresults))

shinyUI(fluidPage(
  titlePanel("Shiny Happy MDA"),
  tags$head(tags$link(rel="stylesheet", type="text/css", href="mda.css")),

  sidebarLayout(
    sidebarPanel(
      # bookmarking probably requires some more setup, or maybe it fails because of the initial error
      # in the app...?
      # bookmarkButton(),
      fileInput("csv", "Upload your own data:",
                accept=c(
                  "text/csv",
                  "text/comma-separated-values"
                )
      ),
      selectInput("results", "Results:", choices=fresults, selected=fresults[1]),
      selectInput("fx", "X axis:", choices=c()),
      selectInput("fy", "Y axis:", choices=c()),
      checkboxGroupInput("mode", "Mode:", choices=c()),
      checkboxGroupInput("division", "Division:", choices=c()),
      h4("Selected chunk:"),
      htmlOutput("click_info"),
      textInput("cql", "CQL:", value='[lemma=".*"]'),
      actionButton("search", "Search", onclick="kontextSearch()"),
      tags$script(src="kontextSearch.js")
    ),

    mainPanel(
      id="main-panel",
      tabsetPanel(
        tabPanel(
          "Factors",
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
          "Loadings",
          plotOutput("lplot")
        ),
        tabPanel(
          "BiberPlot™",
          br(),
          div(
            id="percentile-range-wrapper",
            sliderInput("range", "Percentiles:", min=0, max=1, value=c(.3, .7), width="100%")
          ),
          plotOutput("d1plot"),
          plotOutput("d2plot")
        )
      )
    )
  )
))
