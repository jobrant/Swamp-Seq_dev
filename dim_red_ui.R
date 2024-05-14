dim_red_ui <- function(id) {
  ns <- NS(id)
  tabPanel(
    value = id,
    title = div(tags$strong("Dimensionality Reduction")),
    sidebarLayout(
      sidebarPanel(
        width = 4,
        awesomeRadio(
          inputId = ns("pval_cutoff"),
          label = 'Filter Expression Data Tables by Adjusted p-value',
          choices = list(1, 0.05, 0.01),
          selected = 0.05,
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
        fluidRow(
          box(
            headerBorder = TRUE,
            title = h6('Principal Component Analysis (PCA)', style = 'font-size:28px'),
            height = 540,
            width = 6,
            plotlyOutput(ns('pcaPlot'))
          )
        ),
        fluidRow(
          box(
            headerBorder = TRUE,
            title = h6('Multidimensional Scaling (MDS)', style = 'font-size:28px'),
            height = 540,
            width = 6,
            plotlyOutput(ns('mdsPlot'))
          )
        )
      )
    )
  )
}
