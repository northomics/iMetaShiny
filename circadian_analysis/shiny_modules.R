# Valid colors are: red, yellow, aqua, blue, light-blue, 
# green, navy, teal, olive, lime, orange, fuchsia, purple, maroon, black.


# _____Display a plot within a box, with options to download as high re --------
# usage

# on ui end
  # BoxSVGPNGUI(id = "plotbox1", name_tag = "ggplot_function_test1", is.ggplot2 = TRUE),
  # BoxSVGPNGUI(id = "plotbox2", name_tag = "ggplot_function_test2", is.ggplot2 = TRUE),
  # BoxSVGPNGUI(id = "plotbox3", name_tag = "ggplot_function_test3", is.ggplot2 = FALSE)
  
# on server end
# plot_object_1 <- qplot(1,1)
  # plot_object_2 <- qplot(1:2,1:2)
# 
# pdf("temp")                                                                                                            
# dev.control('enable')  
# plot(1,1)
# plot_object_3 <- recordPlot()
# dev.off()
# 
# name_tag_1 <- "ggplot_function_test_1"
# name_tag_2 <- "ggplot_function_test_2"
# name_tag_3 <- "ggplot_function_test_3"
# 
# callModule(BoxSVGPNG, "plotbox1", plot_object = plot_object_1, name_tag = name_tag_1,is.ggplot2 = TRUE)                          
# callModule(BoxSVGPNG, "plotbox2", plot_object = plot_object_2, name_tag = name_tag_2,is.ggplot2 = TRUE)
# callModule(BoxSVGPNG, "plotbox3", plot_object = plot_object_3, name_tag = name_tag_3,is.ggplot2 = FALSE)

# ui function
# this name_tag is for labeling
# plot_type = c("ggplot2","plotly", "recordPlot")

BoxSVGPNGUI <- function(id, box_title = NULL, 
                        plot_type = "ggplot2", 
                        collapsed = FALSE,
                        collapsible = FALSE,
                        box_width = 6, 
                        menu_width = 6,
                        plot_width = "100%", 
                        plot_height = "400px"){
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  tagList(
    box(
      title = box_title, # this name
      width = box_width,
      status = "primary", 
      solidHeader = TRUE,
      collapsible = collapsible,
      collapsed = collapsed,
      
      if(plot_type == "ggplot2"| plot_type == "plotly"){
        plotlyOutput(outputId = ns("plot"), width = plot_width, height = plot_height)
      }else if(plot_type == "recordPlot"){
        plotOutput(outputId = ns("plot"), width = plot_width, height = plot_height)
      }else{
        print("wrong plot_type, continue using recordPlot")
        plotOutput(outputId = ns("plot"), width = plot_width, height = plot_height)
      }
      ,
      # only show export menu for ggplot2 and recordPlot type
      if(plot_type == "ggplot2"| plot_type == "recordPlot"){
        tagList(
          column((12 - menu_width)),
          column(menu_width,
                 box(
                   title = "Export figure",
                   width = 12,
                   collapsible = TRUE,
                   collapsed = TRUE,
                   background = "light-blue",
                   textInput(inputId= ns("width"), label="width (inch)", value = 10),
                   textInput(inputId= ns("height"), label="height (inch)", value = 8),
                   textInput(inputId= ns("pointsize"), label="Text size", value = 12),
                   textInput(inputId= ns("resolution"), label="PPI for PNG", value = 300),
                   
                   downloadButton(ns("download_SVG"), 'SVG'),
                   downloadButton(ns("download_PNG"), 'PNG')
                 ) 
          )
        )
      }

    ) 
  )
}



# server function:

# this name_tag is for file name export

BoxSVGPNG <-function(input, output, session, plot_object, name_tag, plot_type){
  
  observe({
    if(plot_type == "ggplot2"){
      output$plot <- renderPlotly({
        ggplotly(plot_object)
      })
      # for SVG download
      output$download_SVG <- downloadHandler(
        filename <- paste0(name_tag,".svg"), 
        content <- function(file) {
          svg(file,
              width = as.numeric(input$width), 
              height = as.numeric(input$height),
              pointsize = as.numeric(input$pointsize),
              bg = "white")
          print(plot_object) # here it has to be "print", "plot" works in some rare cases
          #plot_object
          dev.off()
        }
      )
      # for high resolution PNG download
      output$download_PNG <- downloadHandler(
        
        filename <- paste0(name_tag,".png"), 
        content <- function(file) {
          png(file,res = as.numeric(input$resolution),
              width = as.numeric(input$width), 
              height = as.numeric(input$height), 
              pointsize = as.numeric(input$pointsize),
              units = "in", bg = "white")
          
          #plot_object
          print(plot_object) # here it has to be "print", "plot" works in some rare cases
          dev.off()
        }
      )
      
    }else if(plot_type == "plotly"){
      output$plot <- renderPlotly({
        plot_object
      })

    }else if(plot_type == "recordPlot"){
      output$plot <- renderPlot({
        print(plot_object)
      })
      
      # for SVG download
      output$download_SVG <- downloadHandler(
        filename <- paste0(name_tag,".svg"), 
        content <- function(file) {
          svg(file,
              width = as.numeric(input$width), 
              height = as.numeric(input$height),
              pointsize = as.numeric(input$pointsize),
              bg = "white")
          # this is important, it has to be print, not plot, or nothing, 
          # because, the object is already a plot
          print(plot_object)
          dev.off()
        }
      )
      # for high resolution PNG download
      output$download_PNG <- downloadHandler(
        
        filename <- paste0(name_tag,".png"), 
        content <- function(file) {
          png(file,res = as.numeric(input$resolution),
              width = as.numeric(input$width), 
              height = as.numeric(input$height), 
              pointsize = as.numeric(input$pointsize),
              units = "in", bg = "white")
          # this is important, it has to be print, not plot, or nothing, 
          # because, the object is already a plot
          print(plot_object)
          dev.off()
        }
      )
    }
    
  })
  
}


# _____Display a data table from the analysis result within a box,  --------
# usage



BoxTableUI <- function(id, name_tag, collapsed = FALSE){
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  tagList(
    box(
      title = name_tag,
      status = "success", 
      width = 6,
      solidHeader = TRUE,
      collapsible = TRUE,
      collapsed = TRUE,
      DT::dataTableOutput(ns("displaydatatable"))
    )
  )
}


BoxTable <-function(input, output, session, table_object){
  
  observe({
      output$displaydatatable <- DT::renderDataTable(
        table_object, server = FALSE,filter = 'top', extensions = c('Buttons','Scroller'), 
        options = list(
          autoWidth = TRUE, 
          dom = 'Bfrtip',
          buttons = c('colvis'),
          scrollY = 200,
          scrollX = TRUE)
      ) 
      
 
  })
  
}



# _____tsvCSV file upload and readin ----------------------------------------------

# how to use
# on ui: 
# tsvcsvFileInput("datafile", "User data (.csv format)")
# dataTableOutput("table")

# on server
# datafile_upload <- callModule(tsvcsvFile, "datafile",
#                       stringsAsFactors = FALSE)

# output$table <- renderDataTable({
#   datafile_upload()
# })

# Module UI function
tsvcsvFileInput <- function(id, label = "CSV file") {
  # Create a namespace function using the provided id
  ns <- NS(id)
  tagList(
    
    checkboxInput(ns('header'), 'First line is Header?', TRUE),
    checkboxInput(ns('rowname'), 'First column is rownames?', TRUE),
    
    selectInput(ns('sep'), 'Separator',
                c(Comma=',',
                  Semicolon=';',
                  Tab='\t'),
                '\t'),
    selectInput(ns('quote'), 'Quote',
                c(None='',
                  'Double Quote'='"',
                  'Single Quote'="'"),
                ''),
    fileInput(ns('file'), label,
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
}

# Module server function
tsvcsvFile <- function(input, output, session) {
  # The selected file, if any
  userFile <- reactive({
    # If no file is selected, don't do anything
    validate(need(input$file, message = FALSE))
    input$file
  })
  
  # The user's data, parsed into a data frame
  dataframe <- reactive({
    if(is.null(userFile())){return(NULL) }  
    # this is alway the first line when reading in a file to avoid the error message
    
    if(input$rowname){
      read.delim(userFile()$datapath, header=input$header, sep=input$sep, 
                 quote=input$quote, row.names = 1)
    }else{
      read.delim(userFile()$datapath, header=input$header1, sep=input$sep, 
                 quote=input$quote)
    }
  })
  
  # We can run observers in here if we want to
  #bserve({
  #  msg <- sprintf("File %s was uploaded", userFile()$name)
  #  cat(msg, "\n")
  #})
  
  # Return the reactive that yields the data frame
  return(dataframe)
}
