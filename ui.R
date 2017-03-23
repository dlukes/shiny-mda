shinyUI(pageWithSidebar(
  headerPanel(
    "Shiny Happy MDA"
  ),

  sidebarPanel(
    # bookmarking probably requires some more setup, or maybe it fails because of the initial error
    # in the app...?
    # bookmarkButton(),
    fileInput('csv', 'Nahrát vlastní faktory:',
              accept=c(
                'text/csv',
                'text/comma-separated-values'
              )
    ),
    selectInput("fx", "Osa X", choices=c()),
    selectInput("fy", "Osa Y", choices=c()),
    checkboxGroupInput("mode", "Mode", choices=c()),
    checkboxGroupInput("division", "Division", choices=c()),
    h4("Vybraný bod:"),
    htmlOutput("click_info")
    # h4("Brushed points"),
    # verbatimTextOutput("brush_info")
  ),
  
  mainPanel(
    tags$style("#mdaplot{height:90vh !important;}"),
    plotOutput("mdaplot",  # height=720, width=720,
               click="mdaplot_click",
               dblclick="mdaplot_dblclick",
               brush=brushOpts(
                 id="mdaplot_brush",
                 resetOnNew=TRUE
               )
    )
  )
))