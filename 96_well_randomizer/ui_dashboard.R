fluidPage(
  fluidRow(
    column(6,
      box(title = "Step 1: Define your plates",
          status = "primary", 
          width =  12,
          solidHeader = TRUE,
          collapsible = FALSE,
                   radioButtons("plate_featuring", 
                                label = h4("Define your layout here:"), 
                                c("Full 96 well plate" =1, "Partiall plate" =2),
                                selected = 1),
                   hr(),
                   conditionalPanel(
                     condition = "input.plate_featuring == '2'",
                     h4("Remove rows/columns by unchecking:"),
                     checkboxGroupInput("remove_by_rows", label = h5("Rows:"), 
                                        choices = list("A", "B", "C", "D", "E", "F", "G", "H"),
                                        selected = list("A", "B", "C", "D", "E", "F", "G", "H"),
                                        inline   = TRUE),
                     checkboxGroupInput("remove_by_cols", label = h5("Columns:"), 
                                        choices = list("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12"),
                                        selected = list("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12"),
                                        inline   = TRUE)
                     
                   )
      ),
      box(title = "Step 2: Preview plate layout",
          status = "primary",
          width =  12,
          solidHeader = TRUE,
          collapsible = FALSE,
          conditionalPanel(
              condition = "input.plate_featuring == '1'",
              plotOutput("full_plate_pre")
          ),
          conditionalPanel(
              condition = "input.plate_featuring == '2'",
              h4(textOutput("num_sample"),align = "center"),
              plotOutput("partial_plate_pre")
          )
          
      )
    ),
    column(6,
      box(title = "Step 3: Download template table",
          status = "primary",
          width =  12,
          solidHeader = TRUE,
          collapsible = FALSE,
          h3("Tips:"),
          h4("- After seeing desired plate layout in preview, download the template by clicking the button below."),
          h4("- Open the template using Excel, paste/fill in your experimental design in the \"Group\" and \"Sample_ID\" columns. Please note that Sample_ID should be a unique identifier for each sample."),
          h4("- You can paste your design in order, we will randomize it for you."),
          h4("- For better visibility, you want to make the \"Group\" names short, up to four characters is recommended. There is no length limit for \"Sample_ID\"."),
          h3("- Once done, save your table and go to Step 4!"),
          conditionalPanel(
            condition = "input.plate_featuring == '1'",
            downloadButton("full_template", label = "Download template", class = "mybutton"),
            tags$head(tags$style(".mybutton{background-color:#0099ff;color:white;border: none;} .mybutton:hover{background-color:#74c7ff;color:white;border: none;} "))
          ),
          conditionalPanel(
            condition = "input.plate_featuring == '2'",
            downloadButton("part_template", label = "Download template", class = "mybutton"),
            tags$head(tags$style(".mybutton{background-color:#0099ff;color:white;border: none;} .mybutton:hover{background-color:#74c7ff;color:white;border: none;} "))
          )
      ),
      box(title = "Step 4: Upload table",
               status = "primary",
               width =  12,
               solidHeader = TRUE,
               collapsible = FALSE,
               fileInput('file1', h4('Upload your experimental design table:'),
                         accept = c(
                           'text/csv',
                           'text/comma-separated-values',
                           'text/tab-separated-values',
                           'text/plain',
                           '.csv',
                           '.tsv'
                         )
               ),
               conditionalPanel(
                  condition = 'output.fileUploaded1',
                  div(actionBttn("goto_plate",
                                 label = "Generate my plate!",
                                 icon = icon("arrow-right"),
                                 style = "simple",
                                 size = "sm",
                                 color = "primary",
                                 block = TRUE),
                      style="float:right"
                  )
               )
           )
      )
  )
)

