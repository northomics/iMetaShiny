

# data_display_after_upload_UI <- function(id, 
#                                          boxwidth = 8,
#                                          boxtitle = "Upload proteinGroups.txt" ){
#   ns <- NS(id)
#   fluidRow(
#           box(title = "Data table display",
#               status = "primary", 
#               width =  boxwidth,
#               solidHeader = TRUE,
#               collapsible = TRUE,
#               fluidRow(
#                 column(12,
#                        # conditionalPanel(
#                        #   condition = paste0("output['", ns("fileUploaded1"), "']"), # has to use this expression in modules
#                          tableOutput(ns("table_display"))
#                        # )
#                 )
#               )
#           )
#   )
# }

data_uploading <- function(input, output, session) {
  ns <- session$ns
  getData1 <- 
    reactive({
      inFile1 <- input$file1
      if(is.null(inFile1$datapath)){ # only if there the path is a string
        return(NULL) # this is alway the first line when reading in a file
      }else{
        read.delim(inFile1$datapath, header= TRUE, sep= '\t', row.names = 1)
      }
    })

  loaded_file <- reactive({getData1()})
# 
#   if(!is.null(loaded_file)){
#     output$testTable <- DT::renderDataTable(
#       getData1,
#       filter = 'top',
#       extensions = c('Scroller'),
#       options = list(
#         autoWidth = TRUE,
#         pageLength = 50,
#         dom = 'Brtip',
#         scrollY = 600,
#         scrollX = TRUE)
#     )
#   }
# 
#     getData1()# return the expression, not the value
  
}


# Input number of groups
data_grouping_sets_UI <- function(id,
                                  boxwidth = 6,
                                  boxtitle = "Group settings") {
  ns <- NS(id)
       box(
           title = boxtitle,
           width = boxwidth,
           status = "primary",
           solidHeader = TRUE,
           collapsible = TRUE,
           numericInput(ns("number_sets"), "Number of grouping sets:", 2, min = 1, max = 6),
           actionButton(ns("gen_sets"),
                        icon = icon("th-large"),
                        label = "Generate grouping sets",
                        style="color: #fff; background-color: #337ab7; border-color: #2e6da4"
           ),
           br(),
           conditionalPanel(
             condition = 'input.gen_sets > 0',
             ns = ns,
             h4("Input grouping information below:")
           ),
           uiOutput(ns("grouping_sets_tab")),
           uiOutput(ns("grouping_text_tab"))
           )

}

data_grouping_sets <- function(input, output, session) {
  return_number_sets <- reactive({input$number_sets})
  ns <- session$ns
  observeEvent(input$gen_sets, {
    output$grouping_sets_tab <- renderUI({
      num_sets = input$number_sets
      grouping_sets_tab = future_lapply(1:num_sets, function(i){
        tabPanel(paste('Set', i),
                 textInput(ns(paste0('set_label', i)), "Define set label:", paste0('GroupingSet', i)),
                 numericInput(ns(paste0('number_groups', i)), "Number of groups in this set", 3, min = 1, max = 20)
        )
      })
      do.call(tabsetPanel, grouping_sets_tab)
    })
  })
  
  # observeEvent(input$gen_sets, {
  #   output$grouping_sets_tab <- renderUI({
  #     num_sets = input$number_sets
  #     grouping_sets_tab = future_lapply(1:num_sets, function(i){
  #       tabPanel(paste('Set', i),
  #                textInput(ns("set_label"), "Define set label:", paste0('GroupingSet', i)),
  #                numericInput(ns("number_groups"), "Number of groups in this set", 3, min = 1, max = 20)
  #       )
  #     })
  #     do.call(tabsetPanel, grouping_sets_tab)
  #   })
  # })

}

# Select samples for each group ------------------------
select_samples_for_groups_UI <- function(id) {
  ns <- NS(id)
         box(
             title = "Group settings",
             width = 4,
             status = "primary",
             solidHeader = TRUE,
             collapsible = TRUE,
             actionButton(ns("gen_groups"),
                          icon = icon("th-large"),
                          label = "Generate groupings",
                          style="color: #fff; background-color: #337ab7; border-color: #2e6da4"
             ),
             br(),
             conditionalPanel(
               condition = 'input.gen_groups > 0',
               ns = ns,
               h4("Select samples for each group:")
             ),
             column(6,
                    uiOutput(ns("sample_selection_for_grouping"))
             ),
             column(6,
                    uiOutput(ns("sample_text_for_grouping"))
             )
           )
}


select_samples_for_groups <- function(input, output, session, num_groups_in_set) {
  ns <- session$ns
  observeEvent(input$gen_groups, {
    output$sample_selection_for_grouping <- renderUI({
      grouping_sets_tab = future_lapply(1:num_groups_in_set, function(i){
        tabPanel(paste('Set', i),
                 textInput(ns("group_label"), "Group label", paste0('Group', i))
        )
      })
    })
    output$sample_text_for_grouping <- renderUI({
      grouping_sets_tab = future_lapply(1:num_groups_in_set, function(i){
        tabPanel(paste('Set', i),
                 textInput(ns("group_text"), "Type text to match:", paste0('GroupWord', i))
        )
      })
    })
  })
}
# Select samples for each group ends------------------------


# Data upload ----------------------------------------------



