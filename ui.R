fresults <- sort(list.files("results/factors", full.names=TRUE), decreasing=TRUE)
names(fresults) <- tools::file_path_sans_ext(basename(fresults))

shinyUI(fluidPage(
  titlePanel("Shiny Happy MDA"),
  tags$head(tags$link(rel="stylesheet", type="text/css", href="mda.css")),

  sidebarLayout(
    sidebarPanel(
      # bookmarking probably requires some more setup, or maybe it fails because of the initial error
      # in the app...?
      # bookmarkButton(),
      fileInput("csv", "Nahrát vlastní faktory:",
                accept=c(
                  "text/csv",
                  "text/comma-separated-values"
                )
      ),
      selectInput("results", "Výsledky:", choices=fresults, selected=fresults[1]),
      selectInput("fx", "Osa X:", choices=c()),
      selectInput("fy", "Osa Y:", choices=c()),
      checkboxGroupInput("mode", "Mode:", choices=c()),
      checkboxGroupInput("division", "Division:", choices=c()),
      h4("Vybraný bod:"),
      htmlOutput("click_info"),
      textInput("cql", "CQL:", value='[lemma=""]'),
      actionButton("search", "Hledat", onclick="kontextSearch()"),
      tags$script(src="kontextSearch.js")
    ),

    mainPanel(
      id="main-panel",
      tabsetPanel(
        tabPanel(
          "Faktory",
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
        )
      )
    )
  )
))
