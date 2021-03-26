library(DT)
library(shinyjs)

#__ for data prepocess before plotting--------------------------------------
rvalues <- reactiveValues()
rvalues$ready_for_analysis<- FALSE


# Add sweet alert to inform that data has uploaded


#  __ get data_table ------------------------------------------------------
data_table <- reactive({
  if(input$data_source == "2" && input$sample_data >0){
    read.delim("Sample_data.txt", header = TRUE, row.names = 1, check.names = FALSE)
  }else{
    callModule(UPLOAD_TABLE2, "data_table_upload", display_after = FALSE) # call this within the withConsoleRedirect will catch the error message and display
  }
})

output$data_table_upload_status <- reactive({
  return(!is.null(data_table()))
})
outputOptions(output, 'data_table_upload_status', suspendWhenHidden=FALSE)

# display the expression table
observe({
  head(data_table()) # use once
  callModule(DATATABLE_Display, "data_table_display",
             data_table = data_table(),
             filename_tag = "data_table",
             height = 300)
})

# Read meta table
meta_table <- reactive({
  if(input$data_source == "2" && input$sample_data >0){
    read.delim("Sample_data_meta.txt",  header = TRUE, check.names = FALSE, row.names = 1)
  }else{
    callModule(UPLOAD_TABLE2, "meta_table_upload",display_after = FALSE) # call this within the withConsoleRedirect will catch the error message and display
  }
})

# mark the data table status
output$meta_table_upload_status <- reactive({
  return(!is.null(meta_table()))
})
outputOptions(output, 'meta_table_upload_status', suspendWhenHidden=FALSE)


# display the meta table
observe({
  head(meta_table())#  have to "use" this reactive expression once to make it work in the next module
  callModule(DATATABLE_Display, "meta_table_display",
             data_table = meta_table(),
             filename_tag = "meta_table",
             height = 300)
  
})


# # if all two files uploaded
observe({
  if (!is.null(data_table()) && !is.null(meta_table())) {
    try(
      withCallingHandlers({
        #if(colnames()
        sample_names_data_matrix <- colnames(data_table())
        sample_names_data_meta <- rownames(meta_table())
        if(!all(sample_names_data_matrix %in% sample_names_data_meta)){ # if not matched
          message("Grouping information not matched for all the samples! Please check the meta table!")
          stop()
        }else{
          rvalues$ready_for_analysis <- TRUE # if matched
        }
      },
      # can use "warning" instead/on top of "message" to catch warnings too
      message = function(m){
        shinyjs::html("console", m$message, add = TRUE)
      }
      # sendSweetAlert(
      #   session = session,
      #   title = "Data set uploaded",
      #   text = "Please check your data table, pre-process data (optional), and go to the differential expression analysis.",
      #   type = "success"
      # )
      )
    )
  }
})


observe({
  if(rvalues$ready_for_analysis){
    #__ render new menu items once both files are uploaded--------------------------------------
    output$menu2 <- renderMenu({
      sidebarMenu(
        menuItem("Analyze Data", tabName = "Analysis", icon = icon("bar-chart"))
      )
    })
    if(input$if_preprocess_data){
      output$menu1 <- renderMenu({
        sidebarMenu(
          menuItem("Process Data", tabName = "Process", icon = icon("list"))
        )
      })
    }
  }
  
})


# mark the data preparation status
output$data_preparation_status <- reactive({
  return(rvalues$ready_for_analysis)
})
outputOptions(output, 'data_preparation_status', suspendWhenHidden=FALSE)

observe({
  Data_processed <- callModule(datatableProcess_full_server, "data_table_preprocess", data_table())
  # put the returned reacitve expression into new reative values list, for easy downstream useages
  rvalues$Data_processed <- Data_processed()
})

# Swith tabs
observeEvent(
  input$if_preprocess_data, {
    Sys.sleep(0.3)
    
    updateTabItems(session, "tabs", "Process")
  }
)
#Need to add a line to remove the process data tab.

# Swith tabs
observeEvent(
  input$gotoanalysis, {
    updateTabItems(session, "tabs", "Analysis")
  }
)

# Swith tabs
observeEvent(
  input$gotoanalysis2, {
    updateTabItems(session, "tabs", "Analysis")
  }
)
