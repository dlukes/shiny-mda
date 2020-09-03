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
  fresults <- lsResults()
  init_model <- fresults[default_model]
  fresults_names <- names(fresults)
  init_model_seq <- grep(default_model_seq, fresults_names, value=TRUE)

  fluidPage(
    titlePanel("MDAvis"),
    tags$head(tags$link(rel="stylesheet", type="text/css", href="mda.css")),

    sidebarLayout(
      sidebarPanel(
        id="controls-outer",
        width=2,
        div(
          id="controls-inner",
          bookmarkButton(),
          conditionalPanel(condition="input.tabsetPanel != 'MultiModelCmp'",
            fileInput("rdata", "Upload your own data:", accept=".RData"),
            selectInput("results", "Results:", choices=fresults, selected=init_model)
          ),
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
          conditionalPanel(condition="/^(Model|Loadings)Cmp$/.test(input.tabsetPanel)",
            selectInput("cmp_results", "Compare with:", choices=fresults, selected=fresults[2])
          ),
          conditionalPanel(condition="input.tabsetPanel == 'LoadingsCmp'",
            selectInput("dimB", "Dimension B:", choices=c()),
            div(
              class="outer-range-wrapper",
              sliderInput("lc_feats_thresh", "Loading threshold:", min=-1, max=1, step=.05, value=c(-.3, .3), width="100%")
            ),
            checkboxInput("lc_only_feats_in_both", "Only show features present in both models")
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
          ),
          conditionalPanel(condition="input.tabsetPanel == 'Text Types'",
            sliderInput("cluster_k", "Number of clusters:", 3, 15, 10, 1),
            h4("2D clusters plot:"),
            selectInput("cluster_2D_x", "X axis:", choices=c()),
            selectInput("cluster_2D_y", "Y axis:", choices=c()),
            sliderInput("cluster_2D_a", "Periphery cutoff:", .1, 2, .4, .1),
            h4("Cluster means plot:"),
            selectizeInput("cluster_means_dims", "Dimensions:", choices=c(), multiple=TRUE),
            h4("Top N categories in cluster C:"),
            fluidRow(
              column(6, numericInput("cluster_info_topn", "N:", 5)),
              column(6, numericInput("cluster_info_cluster", "C:", 1))
            ),
            h4("Number of chunks per cluster:"),
            tableOutput("clusterSizes")
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
            "ModelCmp",
            plotOutput("modelCmpPlot", height="90vh")
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
            "MultiModelCmp",
            plotOutput("multiModelCmpPlot", height="90vh"),
            tableOutput("multiModelCmpDetails")
          ),
          tabPanel(
            "Feature Critic",
            plotOutput("featCritPlot", height="90vh")
          ),
          tabPanel(
            "TopFeatsPerDim",
            plotOutput("topFeatsPerDimPlot")
          ),
          tabPanel(
            "Text Types",
            h4("2D clusters plot"),
            plotOutput("cluster2DPlot", height="90vh"),
            h4("Cluster means plot"),
            plotOutput("clusterMeansPlot", height="90vh"),
            h4("Top N categories in cluster C"),
            tableOutput("clusterInfo")
          )
        )
      )
    ),
    tags$script(src="kontextSearch.js")
  )
}
