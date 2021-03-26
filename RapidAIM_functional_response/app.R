library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyBS)
library(stringr)
library(colourpicker)
library(plotly)

# The proteinGroup files are large, increnase the limit of file upload size to 500MB
source("shiny_modules_for_RapidAIM.R")
options(shiny.maxRequestSize=500*1024^2)


#  __ get data_table ------------------------------------------------------

data_table <- read.csv("function_for_shiny_log2_n9.csv",header = TRUE, sep=",")

# replace round brackets with rectangle ones
for (i in 8:20) {
    data_table[,i] <- stringr::str_replace_all(data_table[,i], "\\(", "<")
    data_table[,i] <- stringr::str_replace_all(data_table[,i], "\\)", ">")}

for (i in 8:20) {
  data_table[,i] <- stringr::str_replace_all(data_table[,i], "\\[", "<")
  data_table[,i] <- stringr::str_replace_all(data_table[,i], "\\]", ">")}

# Read meta table
meta_table <- read.table("function_for_shiny_meta.txt",header = TRUE, sep="\t")

# COG_cat <- 

#  _header ------------------------------------------------------


header <- dashboardHeader(title = span(img(src="logo_imetalab.png", width = 140), "RapidAIM demo"),
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
    menuItem("About the Dataset", tabName = "dashboard", icon = icon("file")),
    menuItem("Functional analysis", tabName = "Analysis", icon = icon("file")),
    menuItem("Gallery", tabName = "gallery", icon = icon("picture-o")),
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
                        title = "Example: butyrate synthesis pathway",
                        solidHeader = TRUE,
                        status = "primary",
                        width = 6,
                        align = "right",
                        img(src='gallery/RapidAIM_example.png')
                      )
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
  title = "RapidAIM Functional Analysis Demo",
  header,
  sidebar,
  body
)


server <- function(input, output, session) {
  source(file = "server_dashboard.R",
         local = TRUE,
         encoding = "UTF-8")
  source(file = "server_analyze1.R",
         local = TRUE,
         encoding = "UTF-8")
  # output.fileUploaded1 = TRUE
}

shinyApp(ui, server)