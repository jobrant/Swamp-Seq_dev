volcano_ui <- function(id) {
  ns <- NS(id)
  tabPanel(
    value = id,
    title = div(tags$strong("Volcano Plots"), tags$i(style = "color: rgb(0,166,90)")),
    sidebarLayout(
      sidebarPanel(
        width = 4,
        awesomeRadio(
          inputId = ns("RS_pval"),
          label = 'Filter Expression Data Tables by Adjusted p-value',
          choices = list(1, 0.05, 0.01),
          selected = 0.05,
          status = "success"
        ),
        awesomeRadio(
          inputId = ns('RS_logfc'),
          label = 'Filter Expression Data Tables by log2(FC)',
          choices = list(0, 0.58, 1),
          selected = 0.58,
          status = "success"
        )
      ),
      mainPanel(
        h4("Volcano Plots", style = "text-align: center"),
        box(
          width = 11,
          status = "warning",
          solidHeader = TRUE,
          plotlyOutput(ns('RS_plot1'), width = "100%", height = "600px")
        ),
        br(), br(), br(), br(), br(), br(), br(), br(), br(), br(),
        h6("Use the options available on the right side corner to navigate your way on the figure.")
      )
    )
  )
}
