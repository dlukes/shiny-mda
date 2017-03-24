results <- sort(list.files("results/fa", full.names=TRUE), decreasing=TRUE)
names(results) <- tools::file_path_sans_ext(basename(results))

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
      selectInput("results", "Výsledky:", choices=results, selected=results[1]),
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
      plotOutput("mdaplot",
                click="mdaplot_click",
                dblclick="mdaplot_dblclick",
                brush=brushOpts(
                  id="mdaplot_brush",
                  resetOnNew=TRUE
                )
      )
    )
)))
