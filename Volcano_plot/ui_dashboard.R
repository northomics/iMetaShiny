library(shinyWidgets)

fluidPage(
  fluidRow(
    column(4,
      box(title = "Step 1. Upload Data Matrix",
          status = "primary", 
          width =  12,
          solidHeader = TRUE,
          collapsible = FALSE,
          fluidRow(
            column(12,
                   radioButtons("data_source", 
                                label = h3("To start:"), 
                                c("Upload your data" =1, "Check out our sample dataset" =2),
                                selected = 1),
                   conditionalPanel(
                     condition = "input.data_source == '1'",
                     UPLOAD_TABLE_UI2("data_table_upload", 
                                      boxwidth = 12, 
                                      check.names =  FALSE, 
                                      solidHeader = FALSE,
                                      collapsible = FALSE,
                                      status = NULL,
                                      boxtitle = "1) Upload the data matrix file:",
                                      rownames = TRUE), # this is for data file
                     
                     UPLOAD_TABLE_UI2("meta_table_upload", 
                                      boxwidth = 12, 
                                      check.names =  FALSE, 
                                      solidHeader = FALSE,
                                      collapsible = FALSE,
                                      status = NULL,
                                      boxtitle = "2) Upload the meta file:",
                                      rownames = TRUE) # this is for meta file
                   ),
                   conditionalPanel(
                     condition = "input.data_source == '2'",
                     box(title = "Try our sample data:",
                         width =  12,
                         check.names =  FALSE, 
                         solidHeader = FALSE,
                         collapsible = FALSE,
                         status = NULL,
                         "The sample data set contains a data matrix, and a meta file.",
                         br(),
                         "For data matrix, columns are samples and rows are features.",
                         br(),
                         "For meta file, first column is samples and second column is the group information.",
                         br(),
                         img(src='gallery/volcano.png', align = "right")
                     ),
                     box(
                         width =  12,
                         status = "primary", 
                         actionButton("sample_data", 
                                      icon = icon("list-alt"),
                                      label = "Load sample data",
                                      style="float:left; color: #fff; background-color: #337ab7; border-color: #2e6da4"
                         )
                     ),
                     column(12,
                         "(click 'Load sample data' above to load,",
                         "you can try pre-process sample by clicking 'Process data before analysis' switch,",
                         "or directly click 'Go to differential expression analysis' to continue)"
                         
                     )
                   
                   )
            )
            
          )
      ),
      conditionalPanel(
        condition = "output.data_table_upload_status",
        shinydashboard::box(
          title = "Step 3. Process data",
          width =  12,
          status = "primary", 
          solidHeader = TRUE,
          collapsible = FALSE,
          strong("If your data needs processing before analysis:"),
          hr(),
          switchInput(
            inputId = "if_preprocess_data",
            label = "Process data before analysis?",
            labelWidth = "180px",
            onStatus = "primary",
            offStatus = "danger"
          ),
          strong("Tips for pre-processing: please be sure that you aware of what you are doing in order to obtain correct statistical results. Negative and NA values can lead to a failure of the analysis."),
          hr()
          # conditionalPanel(
          #   condition = 'input.if_preprocess_data',
          #   datatableProcess_full_UI("data_table_preprocess")
          # )
        ),
        shinydashboard::box(
          title = "Skip to Step 4. Go to analysis",
          width =  12,
          status = "primary", 
          solidHeader = TRUE,
          collapsible = FALSE,
          strong("If your data is ready to go:"),
          hr(),
          actionButton("gotoanalysis", 
                       icon = icon("arrow-right"),
                       label = "Go to differential expression analysis!",
                       style="float:left; color: #fff; background-color: #337ab7; border-color: #2e6da4"
          )
        )
      )
      
    ),
    # conditionalPanel(
    #   condition = 'output.fileUploaded1',
          column(8,
            box(title = "Step 2. Double-check Data Matrix",
                status = "primary", 
                width =  12,
                solidHeader = TRUE,
                collapsible = TRUE,
                conditionalPanel(
                  condition = "output.data_table_upload_status",
                  DATATABLE_Display_UI("data_table_display",boxwidth = 12,
                                       boxtitle = "Data matrix")
                ),
                conditionalPanel(
                  condition = "output.meta_table_upload_status",
                  DATATABLE_Display_UI("meta_table_display",
                                       boxwidth = 12,
                                       boxtitle = "Meta data"
                  )
                )
            )
        )

  )
)

