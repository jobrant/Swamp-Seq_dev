enrich_ui <- function(id) {
  ns <- NS(id)
  tabPanel(
    value = id,
    title = div(tags$strong("Enrichment Analysis")),
    sidebarLayout(
      sidebarPanel(
        width = 3,
        awesomeRadio(
          inputId = ns("pval_cutoff"),
          label = 'Filter Expression Data Tables by Adjusted p-value',
          choices = list(1, 0.05, 0.01),
          selected = 0.01,
          status = "success"
        ),
        awesomeRadio(
          inputId = ns('logfc_cutoff'),
          label = 'Filter Expression Data Tables by log2(FC)',
          choices = list(0, 0.58, 1),
          selected = 0.58,
          status = "success"
        )
      ),
      mainPanel(
        h3("Enrichment Analysis"),
        fluidRow(
          column(
            width = 6,
            h4("GO Enrichment: Up-regulated Genes"),
            plotlyOutput(ns('GO_up_plot'))
          ),
          column(
            width = 6,
            h4("GO Enrichment: Down-regulated Genes"),
            plotlyOutput(ns("GO_down_plot"))
          )
        ),
        fluidRow(
          column(
            width = 6,
            h4("Enriched GO Terms: Up-regulated Genes"),
            DT::DTOutput(ns('GO_up_table'))
          ),
          column(
            width = 6,
            h4("Enriched GO Terms: Down-regulated Genes"),
            DT::DTOutput(ns('GO_down_table'))
          )
        )
      )
    )
  )
}
