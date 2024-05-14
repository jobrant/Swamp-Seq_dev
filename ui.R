library(shiny)
library(shinyWidgets)
library(plotly)
library(DT)
library(bslib)
library(shinydashboard)
library(shinydashboardPlus)

source('FUNCTIONS/homepage.R')
source('modules/preprocess_ui.R')
source('modules/volcano_ui.R')
source('modules/dim_red_ui.R')
source('modules/enrich_ui.R')

ui <- navbarPage(
  id="MainNavBar", 
  position = "fixed-top",
  theme = bs_theme(bootswatch = "darkly", base_font = font_google("Inter")),
  tags$script(HTML("
    var header = $('.navbar > .container-fluid');
    var navbarHeight = $('.navbar').height();
    header.append('<div style=\"float:right; height:' + navbarHeight + 'px;\"><a href=\"https://cancer.ufl.edu/\"><img src=\"UFH_CancerCenter.jpg\" alt=\"alt\" style=\"width:100%; height:100%;\"></a></div>');
  ")),
  title = tags$strong("SWAMP-SEQ dev version"),
  br(), br(), br(), br(),
  tabPanel(value = "home", 
           title = tags$strong("Home"),
           wellPanel(style = "background-color: rgb(34, 34, 34) !important;text-align: center;",
                     HTML('<h2 style="font-size: 40px;">Welcome to the SWAMP-SEQ Visualization Toolkit <img src="cropped.gif" style="width:100px;height:85px;"></h2>')),
           br(),
           homepage_info,
           br(), br(), br(), br(),
           h5("Instructions"),
           h6("This is an automated do your own RNA-Seq Visualization toolkit..."),
           br(), br(),
           nav_card_home
  ),
  navbarMenu(
    title = div(tags$strong("Process-Data"), tags$i(style = "color: rgb(0,166,90)")),
    tabPanel(preprocess_ui("upload")),
    tabPanel(preprocess_ui("server"))
  ),
  tabPanel(volcano_ui("volcano_plots")),
  tabPanel(dim_red_ui("dimentionality_reduction")),
  tabPanel(enrich_ui("enrich_analysis")),
  tabPanel(value = "info", title = div(tags$strong("About")),
           sidebarLayout(
             sidebarPanel(width = 1 , id = "mySidebar"),
             mainPanel(width = 8, offset = 1,
                       h3("About this toolkit:", style = "color:white; font-weight: 50"),
                       p("This is an interactive visualization toolkit..."),
                       h3("Contact:", style = "color:white; font-weight: 50"),
                       p("This toolkit is developed by Dr. Jason O Brant and Kalyanee Shirlekar, BCB-UF Health Cancer Center. If you have any trouble accessing the content on this application or have any questions or feedback related to the functionality, you can contact us here:"),
                       tags$li("jobrant@ufl.edu"),
                       tags$li("kshirlekar@ufl.edu")
             )
           )
  )
)
