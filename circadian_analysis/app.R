## app.R ##
# circadian analysis

# >run preparation -------------------------------------------------------------

# this is the option for my project, universal
options(stringsAsFactors = FALSE)
options(scipen=10)
options(shiny.maxRequestSize=30*1024^2) # change default maximun size from default 5M to 30M


source("All_combined_subfunctions.r")
source("subfunctions_update.r")
source("shiny_modules.R")
#install.packages(shinysky)  # install but do not library
# do not load the whole shinysky library, which will make the buttons ugly
install.packages.auto(shinydashboard)
install.packages.auto(shiny)
install.packages.auto(htmlwidgets)
install.packages.auto(DT) # for table
install.packages.auto(ggplot2)
install.packages.auto(plotly)
install.packages.auto(colourpicker)
#install.packages.auto(d3heatmap) # for heatmap
install.packages.auto(reshape2)
#install.packages.auto(GGally) # for paried
#install.packages.auto(ggcorrplot) # for correlation plot
install.packages.auto(gplots) # heatmap.2
install.packages.auto(MetaCycle)
install.packages.auto(ggfortify)
install.packages.auto(ComplexHeatmap)  # use this for heatmap plot in the future
#library(ComplexHeatmap)

# Valid colors for shinydashiboard are: 
#   red, yellow, aqua, blue, light-blue, green, navy, teal, olive, lime, orange,
#   fuchsia, purple, maroon, black.


#  _header ------------------------------------------------------

header <- dashboardHeader(title = span(img(src="logo_imetalab.png", width = 150), "Circadian Analysis"),
                          titleWidth = 400,
                          tags$li(class = "dropdown",
                                  tags$a(href="www.imetalab.ca", target="_blank", 
                                         tags$img(height = "18px", alt="SNAP Logo", src="logo_M.png")
                                  )
                          )
)

#  _side bar ------------------------------------------------------

sidebar <- dashboardSidebar(
  width = 250,
  sidebarMenu(
    id = "tabs",
    menuItem("Upload Data", tabName = "dashboard", icon = icon("dashboard")),
    sidebarMenuOutput("menu"),
    menuItem("Gallery", tabName = "gallery", icon = icon("picture-o")),
    menuItem("About", tabName = "about", icon = icon("info-circle")),
    menuItem("iMetaLab", icon = icon("home"), 
             href = "http://www.imetalab.ca")
    
  )
)


# _body --------------------------------------------------------------

body <- dashboardBody(
  tags$head(tags$link(rel = "shortcut icon", href = "logo_M.png")), # for favorcon
  title = "Circadian Analysis",
  tabItems(
    #  __ Panel: file upload ------------------------------------------------------
    tabItem(tabName = "dashboard",
            
            fluidRow(
              
               box(solidHeader = TRUE,
                   title = "Start from here", 
                   status = "primary", 
                   width = 4,
                   #height = 381,
                   "File format has to be tsv or csv. Row as variables, column as experiments", 
                   br(), 
                   "First line as headers, second line as time pints", 
                   br(), 
                   "Replicates are allowed with the same time points markers as the second line", 
                   br(), 
                   "Please check if files are read in correctly after upload.",
                   br(),
                   "Sample files for download (right click to save as): ", 
                   a(href = 'Example2_data.txt', 'sample data for circadian; '),
                   br(),
                   actionButton("goto_gallery", 
                                icon = icon("picture-o"),
                                label = "Gallery",
                                style="color: #383535; background-color: #fff; border-color: #fff"
                   ),
                   
                   br(),
                     radioButtons("dataSelection", 
                                  label = h4("Starting with Data Set"), 
                                  choices = c("Upload Data" = 1, 
                                              "Sample data with even time pointsre and plicates" = 2,
                                              "Sample data with uneven time points" = 3
                                            
                                              )
                                  )
                   ,
                   conditionalPanel(
                     condition = 'output.fileUploaded1',
                       actionButton("goto_data_filter", 
                                    icon = icon("arrow-right"),
                                    label = "Go Analysis",
                                    style="float:right; color: #fff; background-color: #337ab7; border-color: #2e6da4"
                       )
                   )
            
               ),
                 
              
              # _____ box for upload datamatirx --------------------------------------
              conditionalPanel(
                condition = "input.dataSelection == 1",
                box(title = "Upload data matrix (tsv/csv)",
                    status = "primary", 
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    width = 8,
                    tagList(
                      h5("First column as row names"),
                      h5("First line as column/experiment header, Second line as time point(replicates allowed)"),

                      selectInput('sep1', 'Separator',
                                  c(Comma=',',
                                    Semicolon=';',
                                    Tab='\t'),
                                  '\t'),
                      selectInput('quote1', 'Quote',
                                  c(None='',
                                    'Double Quote'='"',
                                    'Single Quote'="'"),
                                  ''),
                      fileInput('file1', 'Data matrix file: ',
                                accept = c(
                                  'text/csv',
                                  'text/comma-separated-values',
                                  'text/tab-separated-values',
                                  'text/plain',
                                  '.csv',
                                  '.tsv'
                                )
                      )
                    )
                )
              ),
              
              
              # _____ box conditional plot display --------------------------------------
 
              conditionalPanel(
                condition = "input.dataSelection == 2",
                box(
                  title = "sample plot from choosen dataset",
                  status = "primary", 
                  solidHeader = TRUE,
                  #collapsible = TRUE,
                  width = 8,
                  img(src='gallery/circadian_02.PNG', align = "left"),
                  img(src='gallery/circadian_03.PNG', align = "left")
                  
                )
              )
            

            )
            , # fluid row ends
            
            # _____ table output --------------------------------------
            conditionalPanel(
              condition = 'output.fileUploaded1',
              fluidRow(
                box(
                  title = "First round of filtering on Original Data Table",
                  width = 12,
                  #height = 850,
                  status = "primary", 
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  collapsed = FALSE,
                  "Choose rows (by clicking anywhere on the row) and columns (by clicking the rownames on the buttom) to subset the table for plotting.",
                  br(),
                  '"Search box" on the right corner is a quick global row selection tool on row names.',
                  br(),
                  "Use the column visibility button to deal with table of many columns.",
                  br(),
                  #checkboxInput('transpose_table_1', 'Transpose table?', FALSE),
                  DT::dataTableOutput('table1_input')
                  
                )
                ,
                box(
                  title = "Further filtering:",
                  width = 12,
                  status = "primary",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  collapsed = FALSE,
                  DT::dataTableOutput('table1_filtered_input')
                  
                ),
                box(
                  width = 12,
                  status = "primary", 
                  solidHeader = TRUE,
                  actionButton("goto_playwith_datamatrix", 
                               icon = icon("arrow-right"),
                               label = "Go to analysis with filtered data matrix",
                               style="float:right; color: #fff; background-color: #337ab7; border-color: #2e6da4"
                  )
  
                )
              )
            ),
            
            shinysky::busyIndicator(text = "Loading, please wait ...",
                                    img = "ajaxloaderq.gif",
                                    wait = 500) 
            
            
    ),
    

    
    #  __ Panel:  Analysis and visualization------------------------------------------------------
    
    # the ui is mainly generated on the server side, because the data is dynamic

    tabItem(tabName = "tables1",
           fluidRow(
             #  ____tab: Analysis UI ------------------------------------------------------
             column(2,
                    box(
                      title = "Analysis Setting",
                      width = 12,
                      status = "primary", 
                      solidHeader = TRUE,
                      checkboxInput("show_meta2d_help", "Show help on core functions", FALSE),
                      selectInput("log_transform_table_input", "Log tranform?", 
                                  choices = list("No transform" = 1, "log10 tranform" = 2, "log2 tranform" = 3, "log tranform" = 4), 
                                  selected = 1),
                      checkboxInput('matrix_inputation', 'Sequential missing value impuation?', FALSE),
                      conditionalPanel(
                        condition = "input.matrix_inputation",
                        tagList(
                          selectInput("matrix_inputation_NA_type", "What is deemed as NA value",
                                      c("NA", "0")),
                          sliderInput("matrix_inputation_alpha", label = "Alpha/Strength", min = 0,
                                      max = 1, value = 0.9)
                        )
                        
                      ),
                      
                      sliderInput("circadian_min", label = "Minimum period", min = 10, 
                                  max = 50, value = 20, step = 1),
                      sliderInput("circadian_max", label = "Maximum period", min = 10, 
                                  max = 50, value = 28, step = 1),
                      selectInput("circadian_method", label = "Analysis method",
                                  c("ARSER" = "ARS",
                                    "JTK_CYCLE" = "JTK",
                                    "Lomb-Scargle" = "LS"
                                  ),
                                  selected = c("ARS","JTK","LS"),
                                  multiple =  TRUE
                      ),
                      
                      selectInput("circadian_method_pvalue", label = "Combined Pvalue method",
                                  c("Bonferroni correction" = "bonferroni",
                                    "Fisher method" = "fisher"),
                                  selected = "bonferroni"
                      ),
                      
                      
                      
                      actionButton("button_run_circadian_analysis", 
                                   icon = icon("paper-plane"),
                                   label = "Run Analysis",
                                   #style="float:right"
                                   style="float:right; color: #fff; background-color: #337ab7; border-color: #2e6da4"
                      ),
                      
                      
                      conditionalPanel(
                        condition = 'output.circadian_analysis_status',
                        selectInput("circadian_filter_FDR", 
                                    label = "Apply FDR filtering", 
                                    choices = c(0.05, 0.01, 0.001, 0.1, 0.5),
                                    selected = 0.5
                                    )
                        
                      )
                      
                    )
            ) ,
            column(10,

                   conditionalPanel(
                     condition = 'output.circadian_analysis_status',
                     tabBox(
                       id ="tabBox_plottings",
                       width = 12,
                       #height = "950px",
                       #selected = "Workflow 1: start from list",
                       
                       #  ____tab: Analyze result ------------------------------------------------------
                       tabPanel(
                         h4("Result Overview"),
                         fluidRow(
                           uiOutput("ui_box_phase_distribution"),
                           uiOutput("ui_box_overall_heatmap")
                           
                         )
                       ),
                       #  ____tab: Plot by table selection------------------------------------------------------
                       tabPanel(
                         h4("Play by manual selection"),
                         fluidRow(
                           column(5,
                                  box(
                                    title = "Select Rows to Plot:",
                                    width = 12,
                                    status = "primary",
                                    solidHeader = TRUE,
                                    #collapsible = TRUE,
                                    DT::dataTableOutput('result_table_meta_filtered')
                                    
                                  ),
                                  box(
                                    width = 12,
                                    status = "primary",
                                    solidHeader = TRUE,
                                    checkboxInput("result_table_rawdata_scale",
                                                  "Scale Rows bofore plot?",
                                                  value = FALSE
                                    ),
                                    checkboxInput("result_table_plot_show_more_options",
                                                  "Show more option for plot",
                                                  value = FALSE
                                    ),
                                    conditionalPanel(
                                      condition = 'input.result_table_plot_show_more_options',
                                      box(
                                        title = "More general plot options",
                                        width = 12,
                                        status = "primary",
                                        solidHeader = TRUE, 
                                        collapsible = TRUE,
                                        collapsed = TRUE,
                                        textInput("linebar_plot_main_title", "Main title", value = "Circadian Profile"),
                                        textInput("linebar_plot_main_xlabel", "X label", value = "Time Points"),
                                        textInput("linebar_plot_main_ylabel", "Y label", value = ""),
                                        textInput("linebar_plot_main_xtext_rotation", "X text rotation(degrees)", value = 0),
                                        textInput("linebar_plot_main_ytext_rotation", "Y text rotation(degrees)", value = 0),
                                        textInput("linebar_plot_height", "Plot height(px)", value = "400"),
                                        sliderInput("linebar_plot_width", label = "Plot width %", min = 1, 
                                                    max = 100, value = 100)
                                      )
                                    ),
                                    
                                    actionButton("button_smooth_plot", 
                                                 icon = icon("paper-plane"),
                                                 label = "Plot Selected Rows",
                                                 #style="float:right"
                                                 style="float:right; color: #fff; background-color: #337ab7; border-color: #2e6da4"
                                                 
                                    )
                                    
                                  )
                                  
                           ),
                           column(7,
                                  uiOutput("ui_manual_smooth_plot")
                           )
                           
                           
                         )
                       ),
                       #  ____tab: plot by cluster selection ------------------------------------------------------
                       tabPanel(
                         h4("Play with Clustering Analysis"),
                         fluidRow(
                           column(5,
                                  box(
                                    width = 12,
                                    status = "primary",
                                    solidHeader = TRUE,
                                    checkboxInput("result_table_rawdata_scale_for_kmeans",
                                                  "Scale Rows?",
                                                  value = TRUE
                                    ),
                                    uiOutput("ui_slideInput_max_for_kmeans_smooth_plot"),
                                    
                                    checkboxInput("show_more_options_for_heatmap", "Show more option for heatmap plot", value = FALSE),
                                    
                                    
                                    checkboxInput("show_more_options_kmeans",
                                                  "Show more option for kmeans plot",
                                                  value = FALSE
                                    ),
                                    
                                    conditionalPanel(
                                      condition = 'input.show_more_options_for_heatmap',
                                      box(
                                        #title = "More general heatmap plot options",
                                        width = 12,
                                        status = "primary",
                                        solidHeader = TRUE,
                                        #collapsible = TRUE,
                                        #collapsed = TRUE,
                                        textInput("linebar_plot_main_title_heatmap", "Main title", value = "Heatmap of Clusters"),
                                        #textInput("linebar_plot_main_xlabel_heatmap", "X label", value = "Time point"),
                                        numericInput("linebar_rownames_width_heatmap", "Row labeling width(cm)", value = 4,width = "50%"),
                                        numericInput("linebar_rownames_fontsize_heatmap", "Rowlabeling font size",
                                                     min = 2, max = 20, step = 2,value = 10,
                                                     width = "50%"),
                                        numericInput("linebar_gap_size", "Gap width between clusters(mm)",
                                                     min = 2, max = 10, step = 1,value = 4,
                                                     width = "50%"),
                                        textInput("linebar_plot_height_heatmap", "Plot height(px)", value = "650")
                                        
                                      )
                                    ),
                                    
                                    conditionalPanel(
                                      condition = 'input.show_more_options_kmeans',
                                      box(
                                        width = 12,
                                        status = "primary",
                                        solidHeader = TRUE, 
                                        textInput("linebar_plot_main_title_kmeans", "Main title", value = "K-means clusters"),
                                        textInput("linebar_plot_main_xlabel_kmeans", "X label", value = "PC 1"),
                                        textInput("linebar_plot_main_ylabel_kmeans", "Y label", value = "PC 2"),
                                        textInput("linebar_plot_main_xtext_rotation_kmeans", "X text rotation(degrees)", value = 0),
                                        textInput("linebar_plot_main_ytext_rotation_kmeans", "Y text rotation(degrees)", value = 0),
                                        textInput("linebar_plot_height_kmeans", "Plot height(px)", value = "400"),
                                        sliderInput("linebar_plot_width_kmeans", label = "Plot width %", min = 1, 
                                                    max = 100, value = 100)
                                      )
                                    ),
                                    
                                    
                                    actionButton("button_kmeans_cluster", 
                                                 icon = icon("paper-plane"),
                                                 label = "Perform Kmeans cluster",
                                                 #style="float:right"
                                                 style="float:right; color: #fff; background-color: #337ab7; border-color: #2e6da4"
                                                 
                                    )
                                  ),
                                  uiOutput("ui_kmeans_autoplot")
                           ),
                           
                           column(7,
                                  uiOutput("ui_complexheatmap")
                           )
                           
                           # box(
                           #   title = "Full result",
                           #   width = 12,
                           #   status = "primary",
                           #   solidHeader = TRUE,
                           #   collapsible = TRUE
                           # )
                         ),
                         fluidRow(
                           column(5,
                                  conditionalPanel(
                                    condition = 'output.kmeans_cluster_status',
                                    box(
                                      width = 12,
                                      status = "primary",
                                      solidHeader = TRUE,
                                      selectInput("Kmeans_cluster_index",
                                                  "Choose which cluster to check profile",
                                                  c(1:10)
                                                  
                                      ),
                                      checkboxInput("show_more_options_keans_smooth_plot",
                                                    "Show more option for plot",
                                                    value = FALSE
                                      ),
                                      conditionalPanel(
                                        condition = 'input.show_more_options_keans_smooth_plot',
                                        box(
                                          #title = "More general plot options",
                                          width = 12,
                                          status = "primary",
                                          solidHeader = TRUE, 
                                          #collapsible = TRUE,
                                          #collapsed = TRUE,
                                          textInput("linebar_plot_main_title_kmeans_smooth_plot", "Main title", value = "K-means cluster n"),
                                          textInput("linebar_plot_main_xlabel_kmeans_smooth_plot", "X label", value = "Time point"),
                                          textInput("linebar_plot_main_ylabel_kmeans_smooth_plot", "Y label", value = ""),
                                          textInput("linebar_plot_main_xtext_rotation_kmeans_smooth_plot", "X text rotation(degrees)", value = 0),
                                          textInput("linebar_plot_main_ytext_rotation_kmeans_smooth_plot", "Y text rotation(degrees)", value = 0),
                                          textInput("linebar_plot_height_kmeans_smooth_plot", "Plot height(px)", value = "400"),
                                          sliderInput("linebar_plot_width_kmeans_smooth_plot", label = "Plot width %", min = 1, 
                                                      max = 100, value = 100)
                                        )
                                      ),
                                      
                                      
                                      actionButton("button_kcluster_plot_smooth", 
                                                   icon = icon("paper-plane"),
                                                   label = "Plot selected cluster",
                                                   #style="float:right"
                                                   style="float:right; color: #fff; background-color: #337ab7; border-color: #2e6da4"
                                                   
                                      )
                                    )
                                  )
                                  
                           ),
                           column(7,
                                  uiOutput("ui_kmeans_smooth_plot")
                           )
                         )
                       ),
                       
                       #  ____tab: Export Analysis result ------------------------------------------------------
                       tabPanel(
                         h4("Export Result"),
                         fluidRow(
                           box(
                             title = "Export direct analysis result",
                             width = 12,
                             status = "primary",
                             solidHeader = TRUE,
                             collapsible = TRUE,
                             collapsed = TRUE,
                             DT::dataTableOutput('result_table_meta')
                           ),
                           conditionalPanel(
                             condition = 'output.kmeans_cluster_status',
                             box(
                               title = "Export clustered combined result",
                               width = 12,
                               status = "primary",
                               solidHeader = TRUE,
                               collapsible = TRUE,
                               collapsed = TRUE,
                               DT::dataTableOutput('result_filtered_clustered_met2d')
                             ),
                             box(
                               title = "Export clustered raw data",
                               width = 12,
                               status = "primary",
                               solidHeader = TRUE,
                               collapsible = TRUE,
                               collapsed = TRUE,
                               DT::dataTableOutput('result_filtered_clustered_rawdata')
                             ),
                             box(
                               title = "Export clustered raw data mean for each time point ",
                               width = 12,
                               status = "primary",
                               solidHeader = TRUE,
                               collapsible = TRUE,
                               collapsed = TRUE,
                               DT::dataTableOutput('result_filtered_clustered_mean')
                             )
                             
                             
                             
                           )
                           
                         )
                       )
                       
                     )
                     
                   ),
                   # show help here
                   conditionalPanel(
                     condition = 'input.show_meta2d_help',
                     box(
                       width = 12,
                       status = "primary",
                       solidHeader = TRUE,
                       includeHTML("meta2d_help.html")
                     )
                   )
                   )
             
             
             
           ),
           shinysky::busyIndicator(text = "Analyzing, please wait ...",
                                   img = "ajaxloaderq.gif",
                                   wait = 500) 
    ),

    #  __ Panel:  Gallery ------------------------------------------------------
    tabItem(tabName = "gallery",
            fluidRow(

              box(
                title = "correlation matrix",
                solidHeader = TRUE,
                status = "primary", 
                width = 6,
                img(src='gallery/circadian_04.PNG')

              ),
              box(
                title = "boxplot with jitter",
                solidHeader = TRUE,
                status = "primary", 
                width = 4,
                img(src='gallery/circadian_02.PNG')
              ),
              box(
                title = "Denisty plot",
                solidHeader = TRUE,
                status = "primary", 
                width = 4,
                img(src='gallery/circadian_03.PNG')
              )
              
              
            )
    ),
    #  __ Panel:  About ------------------------------------------------------
    tabItem(tabName = "about",
            fluidRow(
              box(
                title = "Circadian Analysis", 
                solidHeader = TRUE,
                status = "primary", 
                width = 12,
                "For Easy circadian analysis and visualization", 
                br(),
                h4("Refernces:"),
                a(href = "https://openwetware.org/wiki/HughesLab:JTK_Cycle", 'JTK_Cycle'),
                br(),
                a(href = "https://academic.oup.com/bioinformatics/article/32/21/3351/2415176/MetaCycle-an-integrated-R-package-to-evaluate", 
                  'MetaCycle: an integrated R package to evaluate periodicity in large scale data'),
                br(),
                
                a(href = "https://cran.r-project.org/web/packages/MetaCycle/MetaCycle.pdf", 'MetaCycle Manual'),
                br()
                
                
                
              ),
              box(
                title = "Contact", 
                solidHeader = TRUE,
                status = "primary", 
                width = 12,
                "Author: Zhibin Ning", 
                br(), 
                "Email: ningzhibin@gmail.com",
                br(), 
                a(href = 'https://groups.google.com/forum/#!forum/lovemetalab', 'Suggestions and bug report')
              ),
              box(
                title = "Change log", 
                solidHeader = TRUE,
                status = "primary", 
                width = 12,
                br(),
                "V0.1: 20171006,online ", 
                br() 

              ),
              box(
                title = "Resources & Cheet sheet", 
                solidHeader = TRUE,
                status = "primary", 
                width = 12,
                column(6, 
                       img(src='display.brewer.all.png', align = "left"),  
                       br(),
                       colourInput(
                         "color_picker_helper", 
                         "", 
                         "grey"
                         #showColour = "background",
                         #palette = "limited"
                       )
                ),
                column(6, 
                       a(href = 'http://colorbrewer2.org/', 'RcolorBrewer interactive reference'), 
                       
                       br(),
                       a(href = 'http://databall.co/shiny/shinyggplot/?utm_source=r-bloggers.com&utm_medium=referral&utm_campaign=blogpost&utm_content=text#tab-8930-1', 'ggplot2 Explorer'), 
                       br(),
                       a(href = 'http://fontawesome.io/icons/', 'Icon library')
                       )
              )
            )
    )
    
  ),
  #   CSS section ignore for analysis------------------------------------------------------ 
  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),
  #Semi-collapsible sidebar
  tags$script(HTML("$('body').addClass('sidebar-mini');"))
 
)

#  _ ------------------------------------------------------

server <- function(input, output, session) {
  
  # _____ get the data from upload or from sample data--------------------------------------
   getData1 <- reactive({
     my_data <- switch(input$dataSelection,
           "1" = {
                   inFile1 <- input$file1
                   if(is.null(input$file1)){return(NULL) }  # this is alway the first line when reading in a file
                     read.delim(inFile1$datapath, header=TRUE, sep=input$sep1, 
                                           quote=input$quote1, row.names = 1)
                   
                  },

           "2" = {
             read.delim("www/Example2_data.txt", header = T, row.names = 1)
           },
           "3" = {
             read.delim("www/Example4_data.txt", header = T, row.names = 1)
           }
           )
    # if(input$transpose_table_1){
    #   my_data <- t(my_data)
    # }
    my_data

   })

   #_____ check the data input status--------------------------------------

  output$fileUploaded1 <- reactive({
    return(!is.null(getData1()))
  })
  outputOptions(output, 'fileUploaded1', suspendWhenHidden=FALSE)
  
  
  output$menu <- renderMenu({
    if (!is.null(getData1())) {
      sidebarMenu(
        menuItem("Circadian Analysis", tabName = "tables1", icon = icon("bar-chart"))
      )
      }
  })
  
  
  #_____ render the table for output--------------------------------------
  
  output$table1_input <- DT::renderDataTable(
       getData1(),
       extensions = c('Buttons'),
       selection = list(target = "row+column"),
       options = list(
         dom = 'Bfrtip',
         #pageLength = 5,
         scrollX = TRUE,
         buttons = I('colvis')
         #buttons = list(list(extend = 'colvis', columns = 1:2))
       )
    )
 
  #_____ get the result from the first round filtering --------------------------------------

  table1_filtered <- reactive({
    if (length(input$table1_input_rows_selected)) {
      rows_keep <- input$table1_input_rows_selected
    }else{
      rows_keep <- input$table1_input_rows_all
    }
    
    if(length(input$table1_input_columns_selected)){
      columns_keep <- input$table1_input_columns_selected
      return(getData1()[rows_keep, columns_keep, drop = FALSE])
    }else{
     return(getData1()[rows_keep, , drop = FALSE]) 
    }

  })

  #_____ display the filtered data and for further filtering --------------------------------------

  output$table1_filtered_input <- DT::renderDataTable(
    table1_filtered(),
    filter = 'top',
    extensions = c('Scroller'),
    options = list(
      #dom = 'Bfrtip',
      autoWidth = TRUE,
      buttons = c('colvis'),
      scrollY = 200,
      scrollX = TRUE
    )
  )
  

  #_____ get the data from second round filltering--------------------------------------
  table1_final <- reactive({
    if (length(input$table1_filtered_input_rows_selected)) {
      rows_keep <- input$table1_filtered_input_rows_selected
    }else{
      rows_keep <- input$table1_filtered_input_rows_all
    }
     return(table1_filtered()[rows_keep, , drop = FALSE]) 
    
  })
  
  #_____ circadian analysis--------------------------------------

  circadian_result <- reactive({
    if(input$button_run_circadian_analysis == 0){ 
        return(NULL)
      }else{
      
      isolate({ 
        
        data_table <- table1_final()
        original_column_names <- colnames(data_table)
        time_points <- data_table[1,]
        
        data_matrix <- switch(input$log_transform_table_input,
                              "1" = {
                                data_table[-1,]
                              },

                              "2" = {
                                log10(data_table[-1,])
                              },
                              "3" = {
                                log2(data_table[-1,])

                              },
                              "4" = {
                                log(data_table[-1,])
                              }

        )
        
        # do imputation
        if(input$matrix_inputation){
          data_matrix[data_matrix == input$matrix_inputation_NA_type]<-NA
          data_matrix<-t(rrcovNA::impSeqRob(t(data_matrix), alpha = input$matrix_inputation_alpha)$x)
        }

        data_table <- rbind("timepoint" = time_points, data_matrix)

      
        
        
        temp_file <- tempfile(fileext=".txt")
        
        write.table(data_table, temp_file,
                    sep = "\t",
                    row.names = TRUE,
                    col.names = FALSE # do not need the column names
                    )

        
        analysis_result <- meta2d(infile = temp_file, 
                           filestyle = "txt", 
                           timepoints = "Line1",
                           minper = input$circadian_min,
                           maxper = input$circadian_max,
                           cycMethod = input$circadian_method, 
                           adjustPhase = "predictedPer",
                           combinePvalue = input$circadian_method_pvalue,
                           outIntegration = "onlyIntegration",
                           outRawData = TRUE,
                           outputFile = FALSE)
        
       # return a list, 
        return(list(analysis_result = analysis_result,# direct output of the analyisis
                    original_column_names = original_column_names, # the orignal column names
                    time_points = time_points # the time points, which might have replicates
                    ))
      })
    }
  })
  
  #_____ check the analysis status
  output$circadian_analysis_status <- reactive({
    return(!is.null(circadian_result()))
  })
  outputOptions(output, 'circadian_analysis_status', suspendWhenHidden=FALSE)
  
  
  #_____ organize the analysis result--------------------------------------
  
  circadian_result_organized <- reactive({

      # take over the result   
      result_meta <- circadian_result()$analysis_result$meta  # only use the meta table 
      
      # eyes on, not necessarily used
      orignal_sample_name <- circadian_result()$original_column_names # take over the column nmaes
      time_points <- circadian_result()$time_points # take over the time points
      
      # parse the column names, to check which method used, and figure out the meta2d combined result  
      col_names <- colnames(result_meta)
      
      # find the column index for different result  
      ARS_columns <- which(grepl("ARS",col_names))
      JTK_columns <- which(grepl("JTK",col_names))
      LS_columns <- which(grepl("LS",col_names))
      meta2d_columns <- which(grepl("meta2d",col_names)) # only combined meta2d columns
      rawdata_columns <- (tail(meta2d_columns, 1)+1):ncol(result_meta) # raw data column index
      analysis_columns <- 1: tail(meta2d_columns, 1) # all analyisis column
      
      
      # all the analysis column for display, in case user would like to download
      result_meta_all <- result_meta[,analysis_columns]
      

      # take over and further format the meta2d result
      # result_met2d <- result_meta[,meta2d_columns]
      # rownames(result_met2d) <- result_meta[,1]
      # colnames(result_met2d) <-gsub("meta2d_", "", colnames(result_met2d))
      
      
      if(length(meta2d_columns) < 5 ){
        # if there is only one analysis done, it is LS, in which case, result is not really combined
        result_met2d <- result_meta[,LS_columns] 
        rownames(result_met2d) <- result_meta[,1]
        colnames(result_met2d) <-gsub("LS_", "", colnames(result_met2d))
        colnames(result_met2d)[4] <- "phase"
        
        
      }else{
        result_met2d <- result_meta[,meta2d_columns]
        rownames(result_met2d) <- result_meta[,1]
        colnames(result_met2d) <-gsub("meta2d_", "", colnames(result_met2d))
      }
      
      
      # take over and further format the raw data 
      result_rawdata <- result_meta[,rawdata_columns]
      rownames(result_rawdata) <- result_meta[,1]
      colnames(result_rawdata) <- orignal_sample_name
      
      
      # filtering uising FDR
      result_met2d_filtered <-  result_met2d[which(result_met2d$BH.Q <= as.numeric(input$circadian_filter_FDR)),]
      result_rawdata_filtered <-  result_rawdata[which(result_met2d$BH.Q <= as.numeric(input$circadian_filter_FDR)),]
      
      
      # pack and return all the useful information and return
      return(list(
        orignal_sample_name = orignal_sample_name,
        time_points = time_points,
        result_meta_all = result_meta_all,
        result_met2d_filtered = result_met2d_filtered,
        result_rawdata_filtered = result_rawdata_filtered

      ))

  })
  
  #_____ display the static primary result --------------------------------------
  
  observe({
    if(!is.null(circadian_result())){
      
      # display the full dataset
      output$result_table_meta <- DT::renderDataTable(
        circadian_result_organized()$result_meta_all,
        server = FALSE,
        filter = 'top', 
        extensions = c('Buttons','Scroller'), 
        options = list(
          autoWidth = TRUE, 
          dom = 'Bfrtip',
          buttons = c('colvis'),
          scrollY = 200,
          scrollX = TRUE)
      )
      
      

      # for easy access, take over and rename, result_met2d_filtered
      result_met2d_filtered <- circadian_result_organized()$result_met2d_filtered
      result_rawdata_filtered <- circadian_result_organized()$result_rawdata_filtered
      
      
      # display the filterd dataset  for row selection
      
      output$result_table_meta_filtered <- DT::renderDataTable(
        result_met2d_filtered,
        filter = 'top',
        extensions = c('Scroller'),
        options = list(
          #dom = 'Bfrtip',
          autoWidth = TRUE,
          buttons = c('colvis'),
          scrollY = 400,
          scroller = TRUE,
          scrollX = TRUE
        )
      ) 
      # %>%
      #   formatStyle(
      #     'Base',
      #     background = styleColorBar(result_met2d_filtered$Base, 'steelblue'),
      #     backgroundSize = '100% 90%',
      #     backgroundRepeat = 'no-repeat',
      #     backgroundPosition = 'center'
      #   )
      
      
      
      
      
      
      
      
      # check the profile of the phase
      p_phase_distribution <- ggplot2_prettier (qplot(result_met2d_filtered$period, result_met2d_filtered$phase),
                                                xlab = "Period",
                                                ylab = "Phase",
                                                maintitle = "Phase and Period Distribution"
      )
      
      callModule(BoxSVGPNG, "phase_distribution",
                 plot_object = p_phase_distribution,
                 name_tag = "phase_distribution",
                 plot_type = "ggplot2")
      
      output$ui_box_phase_distribution <- renderUI({
          BoxSVGPNGUI(id = "phase_distribution",
                      plot_type = "ggplot2",
                      box_width = 6,
                      menu_width = 6
                      #,
                      #plot_width = paste0(input$box_plot_width, "%"),
                      #plot_height = paste0(input$box_plot_height, "px")
          )
      })
      
      
      # overall profile of the filtered raw data
      svg(tempfile())
      dev.control('enable') 
      #heatmap3(as.matrix(result_rawdata_filtered), Colv =  NA, scale = "row")
      
      heatmap.2(as.matrix(result_rawdata_filtered),
                trace = "none",
                #Rowv = input$D3heatmap_Rowv, 
                Colv = NA,
                dendrogram = "row",
                #col = colors,
                col = colorRampPalette(c("navy", "white","firebrick3"))(1024),
                #distfun = function(x, method = input$D3heatmap_Dist){dist(x, method = method)},
                #hclustfun = function(x, method = input$D3heatmap_hclustfun){hclust(x, method=method)},
                scale = "row",
                key = FALSE

      ) # plot
      
      
      p_overall_heatmap <- recordPlot()
      dev.off()
      
      #p_overall_heatmap
      
      callModule(BoxSVGPNG, "overall_heatmap",
                 plot_object = p_overall_heatmap,
                 name_tag = "overall_heatmap",
                 plot_type = "recordPlot")
      
      output$ui_box_overall_heatmap <- renderUI({
        BoxSVGPNGUI(id = "overall_heatmap",
                    plot_type = "recordPlot",
                    box_width = 6,
                    menu_width = 6
                    #,
                    #plot_width = paste0(input$box_plot_width, "%"),
                    #plot_height = paste0(input$box_plot_height, "px")
        )
      })
      

    }
  })
  
  
  #________ generate ui for kmeans cluster input --------------------------------------
  
  observe({
    if(!is.null(circadian_result())){
      # for easy access, take over and rename, result_rawdata_filtered
      result_rawdata_filtered <- circadian_result_organized()$result_rawdata_filtered

      output$ui_slideInput_max_for_kmeans_smooth_plot <- renderUI({

        tagList(
          numericInput("Kmeans_cluster_number", label = "Choose clusters number", min = 2, 
                      max = nrow(result_rawdata_filtered), value = 5, step = 1, width = "50%")
        )
      })

    }
  })
  
  #________ update ui for kmeans  --------------------------------------
  # this is a good example of using one widget to update another widget
  # this case is a bit special that the first ui is generated by server, therefore needs a if to judege
  observe({
    if(!is.null(input$Kmeans_cluster_number)){
      s_options = 1:input$Kmeans_cluster_number
      
      updateSelectInput(session, "Kmeans_cluster_index",
                        choices  = s_options
      )
      
      # updateTextInput(session, "linebar_plot_main_title_kmeans_smooth_plot",
      #                 value = paste0("K-means cluster ", input$Kmeans_cluster_index))
    }

  })
  
  observe({
      updateTextInput(session, "linebar_plot_main_title_kmeans_smooth_plot",
                      value = paste0("K-means cluster ", input$Kmeans_cluster_index))
    
  })
  
  
  
  #________ get the selected rows --------------------------------------
  
  selected_rows_from_table <- reactive({
    if (length(input$result_table_meta_filtered_rows_selected)) {
      rows_keep <- input$result_table_meta_filtered_rows_selected
    }else{
      rows_keep <- c(1,2)
    }
    return(rows_keep)
  })
  


  #_____  plot from manual selection of table rows--------------------------------------
  
  observe({
    
    if(input$button_smooth_plot > 0 ){
      
      isolate({
        # for easy access, take over and rename, result_met2d_filtered and other need data
        result_met2d_filtered <- circadian_result_organized()$result_met2d_filtered
        result_rawdata_filtered <- circadian_result_organized()$result_rawdata_filtered
        time_point <- circadian_result_organized()$time_points
        time_point_f <- factor(as.numeric(time_point))
        
        # calculat the mean of each time point
        result_rawdata_filtered_mean<- t(apply(result_rawdata_filtered, 1, function(x){tapply(x,  time_point_f,  mean)}))
        
        # if user needs to do the scale on rows
        if(input$result_table_rawdata_scale){
          
          result_rawdata_filtered_mean <- t(scale(t(result_rawdata_filtered_mean)))
          
        }
        
        
        # debug
        #print(result_rawdata_filtered_mean)
        print("for debug:")
        #print(input$plot_row_index)
        #print(as.numeric(input$plot_row_index))
        print(selected_rows_from_table())
        
        smooth_plot <- ggsimpleline(data_matrix = result_rawdata_filtered_mean,
                                    index = selected_rows_from_table(),
                                    x_cord = as.numeric(levels(time_point_f)),
                                    spline_smooth = TRUE,
                                    maintitle = input$linebar_plot_main_title,
                                    xlab = input$linebar_plot_main_xlabel,
                                    ylab = input$linebar_plot_main_ylabel,
                                    axis.text.angle.x = as.numeric(input$linebar_plot_main_xtext_rotation),
                                    axis.text.angle.y = as.numeric(input$linebar_plot_main_ytext_rotation),
                                    vertical =  FALSE
                                    )
        

        # plot output
        callModule(BoxSVGPNG, "smooth_plot",
                   plot_object = smooth_plot,
                   name_tag = "smooth_plot",
                   plot_type = "ggplot2")

        output$ui_manual_smooth_plot <- renderUI({
          BoxSVGPNGUI(id = "smooth_plot",
                      plot_type = "ggplot2",
                      box_width = 12,
                      plot_width = paste0(input$linebar_plot_width, "%"),
                      plot_height = paste0(input$linebar_plot_height, "px")
          )

        })

      })
    }

  })
  
  rvalues <- reactiveValues()
  
  #_____ kmeans cluster and plot--------------------------------------
  observe({
      if(input$button_kmeans_cluster > 0){
  
      isolate({
        #print("test")
        # for easy access, take over and rename, result_met2d_filtered and other need data
        result_met2d_filtered <- circadian_result_organized()$result_met2d_filtered
        result_rawdata_filtered <- circadian_result_organized()$result_rawdata_filtered
        time_point <- circadian_result_organized()$time_points
        time_point_f <- factor(as.numeric(time_point))
        
         
        # calculat the mean of each time point
        result_rawdata_filtered_mean<- t(apply(result_rawdata_filtered, 1, function(x){tapply(x,  time_point_f,  mean)}))
        
        
        # if user needs to do the scale on rows
        if(input$result_table_rawdata_scale_for_kmeans){
          result_rawdata_filtered_mean <- t(scale(t(result_rawdata_filtered_mean)))
        }
        
        # put this into reative values
        rvalues$time_point_f <- time_point_f
        rvalues$result_rawdata_filtered_mean <-result_rawdata_filtered_mean
        
        set.seed(100)
        # the column names has to be a bit complex for autoplt
        temp_for_plot <- result_rawdata_filtered_mean
        colnames(temp_for_plot) <- paste0("exp_", colnames(temp_for_plot))
        
        #_______ kmeans cluster--------------------------------------
        kmeans_result  <- kmeans(temp_for_plot, 
               input$Kmeans_cluster_number,
               iter.max = 100, nstart = 50
        )
        
        # put the cluster information into the reactive vluaes
        rvalues$k_clusters <-kmeans_result$cluster
        
        #_______ kmeans plot--------------------------------------
        k_means_autoplot <- autoplot(kmeans_result, 
                                     data = temp_for_plot, 
                                     frame = TRUE) 
        
        # prettier
        k_means_autoplot <- ggplot2_prettier(k_means_autoplot,
                                             maintitle = input$linebar_plot_main_title_kmeans,
                                             xlab = input$linebar_plot_main_xlabel_kmeans,
                                             ylab = input$linebar_plot_main_ylabel_kmeans,
                                             axis.text.angle.x = as.numeric(input$linebar_plot_main_xtext_rotation_kmeans),
                                             axis.text.angle.y = as.numeric(input$linebar_plot_main_ytext_rotation_kmeans)
        )
        
        
        
        # plot output
        callModule(BoxSVGPNG, "k_means_autoplot",
                   plot_object = k_means_autoplot,
                   name_tag = "k_means_autoplot",
                   plot_type = "ggplot2")
        
        output$ui_kmeans_autoplot <- renderUI({
          BoxSVGPNGUI(id = "k_means_autoplot",
                      plot_type = "ggplot2",
                      box_width = 12,
                      plot_width = paste0(input$linebar_plot_width_kmeans, "%"),
                      plot_height = paste0(input$linebar_plot_height_kmeans, "px")
          )
        })
        

        #_______ plot heatmap--------------------------------------
        # use the k clustered cluster to split the heatmap 
        output$complexheatmap <- renderPlot({
          Heatmap(result_rawdata_filtered_mean, 
                  #name = "Expression",  
                  #row_title =input$linebar_plot_main_xlabel_heatmap,
                  row_title_rot = 0,
                  column_title = input$linebar_plot_main_title_heatmap,
                  column_title_side = "top",
                  column_title_gp = gpar(fontsize = 20, 
                                         fontface = "bold"),
                  row_names_max_width = unit(as.numeric(input$linebar_rownames_width_heatmap), "cm"),
                  row_names_gp = gpar(fontsize = input$linebar_rownames_fontsize_heatmap),
                  gap = unit(input$linebar_gap_size, "mm"),
                  cluster_columns = FALSE, 
                  heatmap_legend_param = list(title = "Expression", 
                                              color_bar = "continuous"),
                  split = paste0("Cluster ", kmeans_result$cluster))
          
          
        })
        
        output$ui_complexheatmap <- renderUI({
          box(
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            plotOutput("complexheatmap", 
                       height = as.numeric(input$linebar_plot_height_heatmap))
          )

        })
        
        
        #_______ output the clustered result--------------------------------------
        
        output$result_filtered_clustered_met2d <- DT::renderDataTable(
          cbind(cluster = kmeans_result$cluster, result_met2d_filtered),
          server = FALSE,
          filter = 'top', 
          extensions = c('Buttons','Scroller'), 
          options = list(
            autoWidth = TRUE, 
            dom = 'Bfrtip',
            buttons = c('colvis'),
            scrollY = 200,
            scrollX = TRUE)
        )
        output$result_filtered_clustered_rawdata <- DT::renderDataTable(
          cbind(cluster = kmeans_result$cluster, result_rawdata_filtered),
          server = FALSE,
          filter = 'top', 
          extensions = c('Buttons','Scroller'), 
          options = list(
            autoWidth = TRUE, 
            dom = 'Bfrtip',
            buttons = c('colvis'),
            scrollY = 200,
            scrollX = TRUE)
        )
        output$result_filtered_clustered_mean <- DT::renderDataTable(
          cbind(cluster = kmeans_result$cluster, result_rawdata_filtered_mean),
          server = FALSE,
          filter = 'top', 
          extensions = c('Buttons','Scroller'), 
          options = list(
            autoWidth = TRUE, 
            dom = 'Bfrtip',
            buttons = c('colvis'),
            scrollY = 200,
            scrollX = TRUE)
        )
        
        

        
        
      })
    }
  })
  

  
  #_____ check the analysis status
  output$kmeans_cluster_status <- reactive({
    return(!is.null(rvalues$k_clusters))
  })
  outputOptions(output, 'kmeans_cluster_status', suspendWhenHidden=FALSE)


  #_____ plot the selected cluster --------------------------------------
 
  observe({
    if(input$button_kcluster_plot_smooth >0){
      isolate({

        index_kcluster <- which(rvalues$k_clusters == input$Kmeans_cluster_index)
        
        smooth_plot_kmeans <- ggsimpleline(data_matrix = rvalues$result_rawdata_filtered_mean,
                                    index = index_kcluster,
                                    x_cord = as.numeric(levels(rvalues$time_point_f)),
                                    
                                    spline_smooth = TRUE,
                                    
                                    maintitle = input$linebar_plot_main_title_kmeans_smooth_plot,
                                    xlab = input$linebar_plot_main_xlabel_kmeans_smooth_plot,
                                    ylab = input$linebar_plot_main_ylabel_kmeans_smooth_plot,
                                    axis.text.angle.x = as.numeric(input$linebar_plot_main_xtext_rotation_kmeans_smooth_plot),
                                    axis.text.angle.y = as.numeric(input$linebar_plot_main_ytext_rotation_kmeans_smooth_plot),
                                    
                                    vertical =  FALSE
        )
        
        
        # plot output
        callModule(BoxSVGPNG, "smooth_plot_kmeans",
                   plot_object = smooth_plot_kmeans,
                   name_tag = "smooth_plot_kmeans",
                   plot_type = "ggplot2")
        
        output$ui_kmeans_smooth_plot <- renderUI({
          BoxSVGPNGUI(id = "smooth_plot_kmeans",
                      plot_type = "ggplot2",
                      box_width = 12,
                      plot_width = paste0(input$linebar_plot_width_kmeans_smooth_plot, "%"),
                      plot_height = paste0(input$linebar_plot_height_kmeans_smooth_plot, "px")
          )
          
        })
        
        
        
      })
    }
    
    
    
  })
  
  
  #__ switch tab--------------------------------------
  
  observeEvent(
    input$goto_data_filter, {
      updateTabItems(session, "tabs", "tables1")
    }
  )
  observeEvent(
    input$goto_playwith_datamatrix, {
      updateTabItems(session, "tabs", "tables1")
    }
  )
  observeEvent(
    input$goto_gallery, {
      updateTabItems(session, "tabs", "gallery")
    }
  )
  
  
  
  
  
}




#  App entrance: main  ------------------------------------------------------

ui <- dashboardPage(
  title = "Circadian Analysis", # this is going to be the title in the browser
  header, 
  sidebar, 
  body 
 )
shinyApp(ui, server)



