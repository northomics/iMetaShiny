library(dplyr)
library(ggpubr)
library(stringr)
library(scales)


######## Generate a preview for full plate
df <- data.frame(Well = num_to_well(1:96, plate = 96),
                 Group = 0, Sample_ID = 0)

output$full_plate_pre <- renderPlot({
  raw_map(data = df$Group, well = df$Well, plate = 96) + theme(text = element_text(size=20),legend.position = "none")
})
# Full plate template for download
df_download <- data.frame(Well = num_to_well(1:96, plate = 96), Group = "", Sample_ID = "")

######## Generate a preview for partial plate
df_part <- data.frame(Well = num_to_well(1:96, plate = 96),
                      Group = 0, Sample_ID = 0)

df_part_filter <- reactive({
  df_part_filter <- df_part %>% filter(str_detect(Well, paste(as.character(input$remove_by_rows), collapse = "|"))) %>%
                            filter(str_detect(Well, paste(as.character(input$remove_by_cols), collapse = "|")))
  return(df_part_filter)
})

output$partial_plate_pre <- renderPlot({
  raw_map(data = df_part_filter()$Group, well = df_part_filter()$Well, plate = 96) + theme(text = element_text(size=20),legend.position = "none")
})
# Partial plate template for download
df_part_download <- reactive({
  df_part_download <- df_part_filter()
  df_part_download$Group <- ""
  df_part_download$Sample_ID <- ""
  return(df_part_download)
})

####### Display number of samples
output$num_sample <- renderText({ 
  paste("Number of samples on the plate = ", nrow(df_part_filter()))
})


####### Plate download
# Generate table for download

output$full_template <- downloadHandler(
  filename = function() {
    paste('Well_template_', Sys.Date(), '.csv', sep='')
  },
  content = function(con) {
    write.csv(df_download, con)
  }
)

output$part_template <- downloadHandler(
  filename = function() {
    paste('Well_template_', Sys.Date(), '.csv', sep='')
  },
  content = function(con) {
    write.csv(df_part_download(), con)
  }
)


# Receive file from user upload

getData1 <- reactive({
    inFile1 <- input$file1
    if(is.null(input$file1)){return(NULL) }  # this is alway the first line when reading in a file
    read.delim(inFile1$datapath, header=TRUE, sep=",", row.names = 1)
})
# getData1 = read.delim("www/StreamGraph_data3.txt", header = TRUE, check.names = FALSE, sep= "\t")

# record the status
fileUploaded1_status <- output$fileUploaded1 <- reactive({
  return(!is.null(getData1()))
})

outputOptions(output, 'fileUploaded1', suspendWhenHidden=FALSE)


# generate menu

observe({
  if(!is.null(getData1())){
    #__ render new menu items once both files are uploaded--------------------------------------
    output$menu <- renderMenu({
      sidebarMenu(
        menuItem("Generate plate", tabName = "Analysis", icon = icon("table"))
      )
    })
  }
})


# Swith tabs
observeEvent(
  input$goto_plate, {
    updateTabItems(session, "tabs", "Analysis")
  }
)