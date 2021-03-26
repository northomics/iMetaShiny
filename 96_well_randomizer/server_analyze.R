### For demo
# data_design <- read.delim("123.csv", header = TRUE, sep = ",")
# 
# data_random <- data_design[sample(data_design$Sample_ID),]
# 
# data_ramdomized <- data.frame(data_design$X, data_design$Well, data_random$Group, data_random$Sample_ID)
# colnames(data_ramdomized) <- c("Number","Well","Group","Sample_ID")


data_ramdomized <- reactive({
      set.seed(input$seed_input)
      data_random <- getData1()[sample(nrow(getData1())),]
      data_ramdomized <- data.frame(getData1()$Well, data_random$Group, data_random$Sample_ID)
      colnames(data_ramdomized) <- c("Well","Group","Sample_ID")
      return(data_ramdomized)
})


# display the plates
output$plate_user_uploaded <- renderPlot({
  raw_map(data = as.numeric(factor(getData1()$Group)), well = getData1()$Well, plate = 96)+
    geom_point(aes(color=factor(getData1()$Group)),size=15) +
    scale_color_discrete(name = "Conditions")+
    geom_text(aes(label=factor(getData1()$Group)), size=4)+
    theme(text = element_text(size=20),legend.position = "none") 
})


output$plate_randomized <- renderPlot({
  raw_map(data = as.numeric(factor(data_ramdomized()$Group)), well = data_ramdomized()$Well, plate = 96)+
    geom_point(aes(color=factor(data_ramdomized()$Group)),size=15) +
    scale_color_discrete(name = "Conditions")+
    geom_text(aes(label=factor(data_ramdomized()$Group)), size=4)+
    theme(text = element_text(size=20),legend.position = "none")
})


# display the tables
observe({
  head(getData1()) # use once
  callModule(DATATABLE_Display, "data_table_display",
             data_table = getData1(),
             filename_tag = "design_uploaded",
             height = 300)
})


observeEvent(input$seed_input, {
  callModule(DATATABLE_Display, "data_table_randomized_display",
            data_table = data_ramdomized(),
            filename_tag = "design_randomized",
            height = 300)
})