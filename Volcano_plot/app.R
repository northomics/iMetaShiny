library(shiny)
library(shinydashboard)
library(shinyWidgets)


# The proteinGroup files are large, increnase the limit of file upload size to 500MB
options(shiny.maxRequestSize=500*1024^2)

source("shiny_modules_stats_data_grouping.R")
source("shiny_modules_in_development.R")


#  _header ------------------------------------------------------


header <- dashboardHeader(title = span(img(src="logo_imetalab.png", width = 140), "Differential Protein Analyzer"),
                          titleWidth = 460,
                          tags$li(class = "dropdown",
                                  tags$a(tags$img(height = "18px", alt="SNAP Logo", src="logo_M.png")
                                  )
                          )
)


#  _side bar ------------------------------------------------------

sidebar <- dashboardSidebar(
  width = 250,
  sidebarMenu(
    id = "tabs",
    # menuItem("Introduction", tabName = "introduction", icon = icon("info")),
    menuItem("Upload Data", tabName = "dashboard", icon = icon("file")),
    # https://fontawesome.com/v4.7.0/icons/
    sidebarMenuOutput("menu1"),
    sidebarMenuOutput("menu2"),
    menuItem("Gallery", tabName = "gallery", icon = icon("picture-o")),
    menuItem("About", tabName = "about", icon = icon("question-circle")),
    menuItem("iMetaLab", icon = icon("home"), 
             href = "http://www.imetalab.ca")
    
  )
)


# _body --------------------------------------------------------------

body <- dashboardBody(
  tabItems(
    #  ___dashboard/starting  tab  ------------------------------------------------------
    tabItem(tabName = "dashboard",
            source(
              file = "ui_dashboard.R",
              local = TRUE,
              encoding = "UTF-8"
            )$value)
    ,
    tabItem(tabName = "Process",
            source(
              file = "ui_process.R",
              local = TRUE,
              encoding = "UTF-8"
            )$value),
    tabItem(tabName = "Analysis",
            source(
              file = "ui_analyze.R",
              local = TRUE,
              encoding = "UTF-8"
            )$value),
    tabItem(tabName = "gallery",
            tabItem(tabName = "gallery",
                    fluidRow(
                      box(
                        title = "Volcano plot showing smooth curve threshold",
                        solidHeader = TRUE,
                        status = "primary", 
                        width = 6,
                        img(src='gallery/volcano1.png')
                      ),
                      box(
                        title = "Volcano plot showing right-angle threshold",
                        solidHeader = TRUE,
                        status = "primary", 
                        width = 6,
                        img(src='gallery/volcano2.png')
                      )
                    )
            )
            ),
    tabItem(tabName = "about",
            fluidRow(
              box(
                title = "About Differential Protein Analyzer", 
                solidHeader = TRUE,
                status = "primary", 
                width = 12,
                "Welcome to the beta version of our Differential Protein Analyzer!",
                br(),
                "In this app, we perform parametric/non-parametric hypothesis tests, calculate fold changes and visualize the results using volcano plot",
                br(),
                "Besides classical right-angle cut-off, we introduced smooth curve cut-off, which was inspired by the article by Keilhauer et al. (Mol Cell Proteomics. 2015 Jan; 14(1): 120-135.)",
                "The smooth curve is defined by the following equation: y > curvature / |x-\"Log2FoldChangeCutOff\"| + \"-Log10pValueCutOff\"",
                br(),
                "Parametric test is performed using function t.test(), Non-parametric test is performed using function wilcox.test(), p values are adjusted using function p.adjust(). Result is visualized using R packages \"ggplot2\" and \"ggplotly\"."
              )
            )   
    )
  ),

  #   CSS section ignored for analysis------------------------------------------------------ 
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),
  #Semi-collapsible sidebar
  tags$script(HTML("$('body').addClass('sidebar-mini');"))             
)



# ------ UI ---------------------------
ui <- dashboardPage(
  title = "Differential Protein Analyzer",
  header,
  sidebar,
  body
)


server <- function(input, output, session) {
  source(file = "server_dashboard.R",
         local = TRUE,
         encoding = "UTF-8")
  source(file = "server_analyze.R",
         local = TRUE,
         encoding = "UTF-8")
  output.fileUploaded1 = TRUE
}

shinyApp(ui, server)