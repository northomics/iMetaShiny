# update log

# 20190829 added busy indicator to data table process modules
# 20190827 add a module for volcano plot
# 20190826 revision: UPLOAD_TABLE_UI2, added more options to format the box()
# 20180925 revision for sankeyplot ui, slide input for topN, changed to numericInput
# 20180515 revision: for circos plot(ADJACENCY_MATRIX_LIST_CIRCOS),limit the topN option to 200, and remove values equals or less than zero
# 20180313
# 20180216 update the plotinBox module, to be able to show ggplot2 object statically, instead of using plotly interactive way
# 20180212 add ggrepel for scatter plot
# 20180205 add a module for batch effect explorer
# 20180201 add a module for redirect message to console
# 20180118 add a module for maxquant summary.txt plot; revised the table upload module, to be version 2
          # found an advantage of using module instead of incorporating all codes in the main programe. do not have
          # have to deal with reactive expressions. instead, using all reactive values like regular one
# 20180105 add a 3D scatter plot module
# 20171220 fix a few bugs on sankey and circos plot, and add the automatic topN maximum
# 20171218 add a simplified phylo plot ui, becase the collappsable phylo tree is not compaple to saneky plot. once
# 20171207 add upload table module
# 20171201 add phylo plot module, with support of hirachcal data
# 20171130 add sankey plot module from contigency matrix or data.frame
# 20171127 add circos plot from contigency matrix or data.frame
# 20171126 add matrix profile view
# 20171122 add view matrix and pvalue combined profile display
# 20171122 add DATATABLE displa module
# 20171121 add heatmap module, fully independent
# 20171102 fixed bugs and add reverse color options
# 20171026 color picking module finished
# 


# Known compatible issues
# interactive sankey plot is does not show up once the collappsable tree is loaded, do not use together
# interactive sankey plot is not compatible to d3 treemap, which ever load later does not show up, do not use together


# about d3tree
# there are two packages with d3tree command, 
# one is d3Tree, with latest update @ Jue, 2017, aims to create Interactive Collapsible Trees with the JavaScript 'D3
# the other is d3treeR, with latest update @ 2015, aims to  It is designed to integrate seamlessly with the R treemap package or work with traditional nested JSON hierarchies.
# therefore, these two d3tree are totally different, use d3Tree::d3tree or d3treeR::d3tree  respectively

library(future.apply)


source("subfunctions_general.r") # first
source("subfunctions_general_update.r") # second, with some functions overiding the old version



# plotting box plot with error bars, violin plot, density, , histogram etc
# to be finished
# statistics_plot_UI <- function(id,
#                                layout = "2column"
#                               ){
#   ns <- NS(id)
#   install.packages.auto(colourpicker)
#   
#   switch(layout,
#          "1column" = {setting_panel_width <- 12
#          },
#          "2column" = {setting_panel_width <- 5
#          }
#   )
#   
#   
#   
#   fluidRow(
#     column(
#       setting_panel_width,
#       box(
#         width = 4,
#         status = "primary", 
#         solidHeader = TRUE,
#         fluidRow(
#           column(6,
#                  selectInput("box_plot_type", "Plot type:", c("box","box+jitter", "violin","violin+jitter", "Density", "histogram"), "box"),
#                  selectInput("box_plot_by", "Plot by:", c("Row", "Column"), "Column"),
#                  checkboxInput("box_plot_mono_color", "Mono Color?",value = FALSE),
#                  checkboxInput("box_plot_tranpose", "Transpose(flip overl) figure?",value = FALSE)
#                  
#           ),
#           column(6,                                 
#                  uiOutput("ui_for_histogram_only"),
#                  sliderInput('box_plot_alpha', label='Fill in colour alpha(opaque)', min = 0 , max = 1, value = 0.5, step = 0.1),
#                  
#                  uiOutput("ui_for_mono_color"),
#                  uiOutput("ui_for_density_only"),
#                  textInput("box_plot_main_title", "Main title", value = ""),
#                  textInput("box_plot_main_xlabel", "X label", value = ""),
#                  textInput("box_plot_main_ylabel", "Y label", value = ""),
#                  textInput("box_plot_main_xtext_rotation", "X text rotation", value = 0)
#           )
#         ),
#         
#         fluidRow(
#           column(6),
#           column(6,
#                  actionButton("button_boxplot", 
#                               icon = icon("paper-plane"),
#                               label = "Plot",
#                               style="float:right; color: #fff; background-color: #337ab7; border-color: #2e6da4"
#                  )
#           )
#         )
#         
#       ),
#       br(),
#       # here is the plot output, which is already in box
#       if(layout == "1column"){ # for one colmn layout
#         uiOutput(ns("ui_box_plot"))
#       }
#     
#     # display plot
#     uiOutput("ui_box_plot")
#     )
#   )
#   
# }
# 
# 
# statistics_plot_SERVER <- function(input, output, session,
#                                    data_frame
#                                    ){
#   ns <- session$ns
#   
# }


# note that he KEGG_ID_convert module is not for KO number convertion
# it just uses the KEGG API to do the bio ID converstion

GSEA_GO_UI <- function(id){
  ns <- NS(id)
  

  fluidRow(
    shinyjs::useShinyjs(),# a lot of javascript functions can be used
    box(
      width = 6,
      solidHeader = TRUE,
      status = "primary",
      wellPanel(
      fluidRow(
        
        column(6,
                 selectInput(ns("OrgDb"),
                             "Set Species:",
                             c("Human" = "org.Hs.eg.db", 
                               "Mouse" ="org.Mm.eg.db",
                               "Rat" ="org.Rn.eg.db")
                 ),
                 
                 selectInput(ns("ont"),
                             "Choose analysis type",
                             c("BP", "MF", "CC", "GO"),
                             selected = "BP"
                 ),
                 uiOutput(ns("UI_in_ID_type")),
                 sliderInput(ns("exponent"),
                             "weight of each step",
                             min = 0,
                             max = 1,
                             value = 1
                             ),
                 sliderInput(ns("nPerm"),
                             "Permutation number",
                             min = 100,
                             max = 5000,
                             value = 1000
     
               )
               
        ),
        column(6,
               sliderInput(ns("minGSSize"),
                           "minimal size of each geneSet for analyzing",
                           min = 10,
                           max = 500,
                           value = 10
               ),
               sliderInput(ns("maxGSSize"),
                           "maximal size of genes annotated for testing",
                           min = 10,
                           max = 500,
                           value = 500
               ),
               textInput(ns("pvalueCutoff"),
                         "maximal size of genes annotated for testing",
                         value = 0.05
               ),
               selectInput(ns("pAdjustMethod"),
                           "maximal size of genes annotated for testing",
                           c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none"),
                           selected = "BH"
               )
               )
        )
     
      ),
      conditionalPanel(
        condition = paste0("output['", ns("gene_list_status"), "']"),
        
        fluidRow(
          column(12,
                 actionButton(ns("button_analysis"), 
                              icon = icon("paper-plane"),
                              label = "GESA Analysis",
                              style="float:right; color: #fff; background-color: #337ab7; border-color: #2e6da4"
                 )
          )
        )
      ),
      
      
      br(),
      fluidRow(
        column(12,
               verbatimTextOutput(ns("console"), placeholder = FALSE)
        )
      )
    ),
    # this is the output section
    conditionalPanel(
      condition = paste0("output['", ns("analysis_status"), "']"),
      tabBox(
        width = 6,
        
        tabPanel("Result Download",
                 fluidRow(
                   column(12,
                          DATATABLE_Display_UI(ns("result_table_view_download"))
                   )
                 )
        ),
        tabPanel("Convertion Summary",
                 fluidRow(
                   column(12,
                          uiOutput(ns("ui_analysis_summary"))
                   )
                   
                 )
        )
        
        
      )
    )
  )
}

GSEA_GO_SERVER <-function(input, output, session, df_fc){
  ns <- session$ns
  library("clusterProfiler")
  
  
  id_type <- reactive({
    switch(input$OrgDb,
           "org.Hs.eg.db" = {
             library(org.Hs.eg.db)
             keytypes(org.Hs.eg.db)
           },
           "org.Mm.eg.db" = {
             library(org.Mm.eg.db)
             keytypes(org.Mm.eg.db)
           },
           "org.Rn.eg.db" = {
             library(org.Rn.eg.db)
             keytypes(org.Rn.eg.db)
           }
    )
  })
  
  
  output$UI_in_ID_type <- renderUI({
    selectInput(ns("keytype"),
                "keytype of input gene",
                id_type(),
                selected = "ENTREZID"
    )
  })
  
  
  # makrs the status of data upload
  output$gene_list_status <- reactive({
    !is.null(df_fc)
  })
  outputOptions(output, 'gene_list_status', suspendWhenHidden=FALSE)
  
  
  # _____ do analysis____________
  observe({

    if(input$button_analysis > 0){
      # 
      # if(is.null(input$out_ID_type) ){
      #   shinyjs::html("console", "Error!!!  Output ID type not set yet\n", add = FALSE)
      # }else if(length(gene_list()) == 0){
      #   shinyjs::html("console", "Error!!!  Please input IDs first", add = FALSE)
      # }else{
      #   
        gesa_result <- isolate({
          try(withCallingHandlers(
            gseGO(geneList     = geneList,
                  OrgDb        = org.Hs.eg.db,
                  keyType = "ENTREZID",
                  ont          = "CC",
                  nPerm        = 1000,
                  minGSSize    = 100,
                  maxGSSize    = 500,
                  pvalueCutoff = 0.05,
                  verbose      = TRUE),
            warning = function(m) {
              shinyjs::html("console", m$message, add = TRUE)
            },
            message = function(m) {
              shinyjs::html("console", m$message, add = TRUE)
            }
          )
          )
        })
        
        ##### summary and output
        if(class(gesa_result) != "try-error"){ # if there is no error, otherwise will crash for data.table operation
          
          # dotplot here
          # to do here, with a module
          
          
          
          
          # output$number_id_input <- renderText({ number_id_input})
          # output$number_id_input_matched <- renderText({ number_id_input_matched})
          # output$id_input_not_matched <- renderText({ id_input_not_matched})
          # 
          # 
          # 
          output$ui_analysis_summary <- renderUI({
            tagList(
              h4("Numer of ID input "),
              verbatimTextOutput(ns("number_id_input")),
              h4("Numer of ID matched in the database"),
              verbatimTextOutput(ns("number_id_input_matched")),
              h4("IDs not matched in the datbase"),
              verbatimTextOutput(ns("id_input_not_matched"))
            )
          })
          
          
          try(callModule(DATATABLE_Display,
                         "result_table_view_download",
                         data_table = gesa_result@result,
                         filename_tag = "id_convert")
          )
        }else{
          
          shinyjs::html("console", ids, add = FALSE)
          
        }
      #} 
      
      #_____ check the analysis status
      output$analysis_status <- reactive({
        return(!is.null(gesa_result))
      })
      outputOptions(output, 'analysis_status', suspendWhenHidden=FALSE)
      
    }
    
  })
  
  
  
  
}




KEGG_ID_convert_UI <- function(id){
  ns <- NS(id)
  data(gcSample)
  sample_id_symbol_human <- gcSample[[1]]
  sample_id_symbol_human <- paste0(sample_id_symbol_human, collapse ="\n")
  
  fluidRow(
    shinyjs::useShinyjs(),# a lot of javascript functions can be used
    box(
      width = 6,
      solidHeader = TRUE,
      status = "primary",
      fluidRow(
        column(8,
               wellPanel(
                 conditionalPanel(
                   condition = paste0("output['", ns("checbox_not_checked"), "']"),
                   selectInput(ns("species_select"),
                               "Set Species:",
                               c("human"="hsa", 
                                 "Mouse" = "mmu",
                                 "Rat" = "rno" )
                   )
                   
                 ),
                 checkboxInput(ns("not_on_the_list"),
                               "My species is not on the list"
                               ),
                 conditionalPanel(condition = paste0("input['", ns("not_on_the_list"), "']"),
                                  #strong("Set Species:"),
                                  textInput(ns("species_typein"),
                                            label = "Set Species:",
                                            value = "hsa"
                                            ),
                                  "valid species abbrevation can be found", 
                                  a(href = 'https://www.genome.jp/kegg/catalog/org_list.html', 'here')
                                  )
               ),
               wellPanel(
                 uiOutput(ns("UI_in_ID_type")),
                 uiOutput(ns("UI_out_ID_type"))
               )
        ),
        column(4,
               wellPanel(
                 textAreaInput(ns("input_gene_list"), 
                               "Gene/protein list",
                               value = sample_id_symbol_human,
                               height = "300px")
               ) 
        )
      ),
      conditionalPanel(
        condition = paste0("output['", ns("gene_list_status"), "']"),
        
        fluidRow(
          column(12,
                 actionButton(ns("button_analysis"), 
                              icon = icon("paper-plane"),
                              label = "Convert",
                              style="float:right; color: #fff; background-color: #337ab7; border-color: #2e6da4"
                 )
          )
        )
      ),
      
      
      br(),
      fluidRow(
        column(12,
               verbatimTextOutput(ns("console"), placeholder = FALSE)
        )
      )
    ),
    # this is the output section
    conditionalPanel(
      condition = paste0("output['", ns("convert_status"), "']"),
      tabBox(
        width = 6,
        
        
        tabPanel("Result Download",
                 fluidRow(
                   column(12,
                          DATATABLE_Display_UI(ns("result_table_view_download"))
                   )
                 )
        ),
        tabPanel("Convertion Summary",
                 fluidRow(
                   column(12,
                          uiOutput(ns("ui_analysis_summary"))
                   )
                   
                 )
        )
        
        
      )
    )
  )
}

KEGG_ID_convert_SERVER <-function(input, output, session){
  ns <- session$ns
  library("clusterProfiler")
  
  output$UI_in_ID_type <- renderUI({
    selectInput(ns("in_ID_type"),
                "Choose input ID type",
                c("kegg", "ncbi-geneid", "ncbi-proteinid","uniprot"),
                selected = "kegg"
    )
  })
  
  output$UI_out_ID_type <- renderUI({
    selectizeInput(ns("out_ID_type"),
                   "Choose output ID type",
                   c("kegg", "ncbi-geneid", "ncbi-proteinid","uniprot"),
                   selected = "uniprot",
                   #multiple = TRUE,
                   options = list(plugins=list('drag_drop','remove_button')
                   )
    )
  })
  
  # get the input ids and filter out any invalid element
  
  gene_list <- reactive({
    all_ids <- unlist(strsplit(input$input_gene_list, "\n"))
    all_ids <- all_ids[(all_ids != "")]# remove any empty/void element
    unique(all_ids)
    
  })
  
  # record the status of data upload
  output$gene_list_status <- reactive({
    length(gene_list()) > 0
  })
  outputOptions(output, 'gene_list_status', suspendWhenHidden=FALSE)
  
  
  # _____ do convert____________
  observe({
    
    if(input$not_on_the_list){
      organism_input <- input$species_typein
    }else{
      organism_input <- input$species_select
    }
    
    
    if(input$button_analysis > 0){
      if(is.null(input$out_ID_type) ){
        shinyjs::html("console", "Error!!!  Output ID type not set yet\n", add = FALSE)
      }else if(length(gene_list()) == 0){
        shinyjs::html("console", "Error!!!  Please input IDs first", add = FALSE)
      }else{
        
        ids <- isolate({
          try(withCallingHandlers(
              bitr_kegg(gene_list(), 
                        fromType=input$in_ID_type, 
                        toType = isolate(input$out_ID_type),
                        organism = organism_input,
                        drop = TRUE
              ),
              warning = function(m) {
                shinyjs::html("console", m$message, add = FALSE)
              },
              message = function(m) {
                shinyjs::html("console", m$message, add = FALSE)
              }
            )
          )
        })
        
        
        ##### summary and output
        if(class(ids) != "try-error"){ # if there is no error, otherwise will crash for data.table operation
          
          number_id_input <- length(gene_list())
          number_id_input_matched <- length(unique(ids[,1]))
          
          
          if(number_id_input > number_id_input_matched ){
            id_input_not_matched <- gene_list()[!(gene_list() %in% unique(ids[,1]))]
          }else{
            id_input_not_matched <- 0
          }
          
          output$number_id_input <- renderText({ number_id_input})
          output$number_id_input_matched <- renderText({ number_id_input_matched})
          output$id_input_not_matched <- renderText({ id_input_not_matched})
          
          # 
          # 
          output$ui_analysis_summary <- renderUI({
            tagList(
              h4("Numer of ID input "),
              verbatimTextOutput(ns("number_id_input")),
              h4("Numer of ID matched in the database"),
              verbatimTextOutput(ns("number_id_input_matched")),
              h4("IDs not matched in the datbase"),
              verbatimTextOutput(ns("id_input_not_matched"))
            )
          })
          
          
          try(callModule(DATATABLE_Display,
                         "result_table_view_download",
                         data_table = ids,
                         filename_tag = "id_convert")
          )
        }else{
          
          shinyjs::html("console", ids, add = FALSE)
          
        }
      } 
      
      #_____ check the analysis status
      output$convert_status <- reactive({
        return(!is.null(ids))
      })
      outputOptions(output, 'convert_status', suspendWhenHidden=FALSE)
      
      
    }
    

    
    

    
    
    #_____ return the checkbox button check status to the browser
    output$checbox_not_checked <- reactive({
      !input$not_on_the_list
    })
    outputOptions(output, 'checbox_not_checked', suspendWhenHidden=FALSE)
    
    
    
    
  })
  
  
  
  
}



BioID_convert_UI <- function(id){
  ns <- NS(id)
  sample_id_symbol_human <- c("GPX3",  "GLRX",   "LBP",   "CRYAB", "DEFB1", "HCLS1",   "SOD2",   "HSPA2",
         "ORM1",  "IGFBP1", "PTHLH", "GPC3",  "IGFBP3","TOB1",    "MITF",   "NDRG1",
         "NR1H4", "FGFR3",  "PVR",   "IL6",   "PTPRM", "ERBB2",   "NID2",   "LAMB1",
         "COMP",  "PLS3",   "MCAM",  "SPP1",  "LAMC1", "COL4A2",  "COL4A1", "MYOC",
         "ANXA4", "TFPI2",  "CST6",  "SLPI",  "TIMP2", "CPM",     "GGT1",   "NNMT",
         "MAL",   "EEF1A2", "HGD",   "TCN2",  "CDA",   "PCCA",    "CRYM",   "PDXK",
         "STC1",  "WARS",  "HMOX1", "FXYD2", "RBP4",   "SLC6A12", "KDELR3", "ITM2B")
  sample_id_symbol_human <- paste0(sample_id_symbol_human, collapse ="\n")
  
  fluidRow(
    shinyjs::useShinyjs(),# a lot of javascript functions can be used
    box(
      width = 6,
      solidHeader = TRUE,
      status = "primary",
      fluidRow(
        column(4,
               wellPanel(
                 selectInput(ns("species"),
                             "Choose Species:",
                             c("Human" = "org.Hs.eg.db", 
                               "Mouse" ="org.Mm.eg.db",
                               "Rat" ="org.Rn.eg.db")
                 ),
                 
                 uiOutput(ns("UI_in_ID_type")),
                 uiOutput(ns("UI_out_ID_type"))
               )
        ),
        column(8,
               wellPanel(
                 textAreaInput(ns("input_gene_list"), 
                               "Gene/protein list",
                               value = sample_id_symbol_human,
                               height = "300px")
               ) 
               
        )
        
        #)
      ),
      
      
      fluidRow(
        column(12,
               actionButton(ns("button_analysis"), 
                            icon = icon("paper-plane"),
                            label = "Convert",
                            style="float:right; color: #fff; background-color: #337ab7; border-color: #2e6da4"
               )
        )
      ),
      br(),
      fluidRow(
        
        column(12,
               verbatimTextOutput(ns("console"), placeholder = FALSE)
        )
        
        
      ),
      fluidRow(
        column(12,
               wellPanel(
                 "Supported species/database can be found", 
                 a(href = 'https://bioconductor.org/packages/3.7/data/annotation/', 'here'),
                 br(),
                 "let the administrator know to download the database to support your species."
               )
               )
               
      )

    ),
    #uiOutput(ns("ui_plot"))
    # this is the output section
    conditionalPanel(
      condition = paste0("output['", ns("convert_status"), "']"),
      tabBox(
        width = 6,


        tabPanel("Result Download",
                 fluidRow(
                   column(12,
                          DATATABLE_Display_UI(ns("result_table_view_download"))
                   )
                 )
        ),
        tabPanel("Convertion Summary",
                 fluidRow(
                   column(12,
                          uiOutput(ns("ui_analysis_summary"))
                   )
                   
                 )
        )
        
        
      )
    )
    )
}

BioID_convert_SERVER <-function(input, output, session){
  ns <- session$ns
  library("clusterProfiler")
  # load the corresponding database, and return the key types supported from that database
  id_type <- reactive({
    switch(input$species,
           "org.Hs.eg.db" = {
            library(org.Hs.eg.db)
             keytypes(org.Hs.eg.db)
           },
           "org.Mm.eg.db" = {
             library(org.Mm.eg.db)
             keytypes(org.Mm.eg.db)
           },
           "org.Rn.eg.db" = {
             library(org.Rn.eg.db)
             keytypes(org.Rn.eg.db)
           }
    )
  })
  
  
  output$UI_in_ID_type <- renderUI({
    selectInput(ns("in_ID_type"),
                   "Choose input ID type",
                   id_type(),
                   selected = "SYMBOL"
                   #multiple = TRUE,
                   #options = list(plugins=list('drag_drop','remove_button')
                   #)
    )
  })
  

  output$UI_out_ID_type <- renderUI({
    selectizeInput(ns("out_ID_type"),
                   "Choose output ID type",
                   id_type(),
                   #selected = 1,
                   multiple = TRUE,
                   options = list(plugins=list('drag_drop','remove_button')
                   )
    )
  })
  
  # get the input ids and filter out any invalid element

  gene_list <- reactive({
    all_ids <- unlist(strsplit(input$input_gene_list, "\n"))
    all_ids <- all_ids[(all_ids != "")]# remove any empty/void element
    unique(all_ids)
    
  })
  
  # record the status of data upload
  output$gene_list_status <- reactive({
    return(length(gene_list()) > 0 )
  })
  outputOptions(output, 'gene_list_status', suspendWhenHidden=FALSE)
  

  
  
  
  # _____ do convert____________
  observe({
    if(input$button_analysis > 0){
      #print(gene_list())
      #print(input$in_ID_type)
      #print(input$out_ID_type)
      #print(input$species)
      
      # ids <- try(bitr(gene_list(), 
      #                 fromType=input$in_ID_type, 
      #             toType = input$out_ID_type,
      #             OrgDb = input$species
      #            ))
      
      if(is.null(input$out_ID_type) ){
        shinyjs::html("console", "Error!!!  Output ID type not set yet\n", add = FALSE)
      }else if(length(gene_list()) == 0){
        shinyjs::html("console", "Error!!!  Please input IDs first", add = FALSE)
      }else{
        try(
          withCallingHandlers(
            ids <- bitr(gene_list(), 
                        fromType=input$in_ID_type, 
                        toType = isolate(input$out_ID_type),
                        OrgDb = input$species
            ),
            message = function(m) {
              shinyjs::html("console", m$message, add = FALSE)
            }
          )
        )
        
        
        
        ##### summary
        
        number_id_input <- length(gene_list())
        number_id_input_matched <- length(unique(ids[,1]))
        
        
        if(number_id_input > number_id_input_matched ){
          id_input_not_matched <- gene_list()[!(gene_list() %in% unique(ids[,1]))]
        }else{
          id_input_not_matched <- 0
        }
        
        output$number_id_input <- renderText({ number_id_input})
        output$number_id_input_matched <- renderText({ number_id_input_matched})
        output$id_input_not_matched <- renderText({ id_input_not_matched})
        
        # 
        # 
        output$ui_analysis_summary <- renderUI({
          tagList(
            h4("Numer of ID input "),
            verbatimTextOutput(ns("number_id_input")),
            h4("Numer of ID matched in the database"),
            verbatimTextOutput(ns("number_id_input_matched")),
            h4("IDs not matched in the datbase"),
            verbatimTextOutput(ns("id_input_not_matched"))
          )
        })
        
        
        try(callModule(DATATABLE_Display,
                       "result_table_view_download",
                       data_table = ids,
                       filename_tag = "id_convert")
        )
        
        
        
        
      } 
      
    }else{
      ids <- NULL# by doing this, the table display will not show error, because ids is defined here
    }
    
    
    
    #_____ check the analysis status
    output$convert_status <- reactive({
      return(!is.null(ids))
    })
    outputOptions(output, 'convert_status', suspendWhenHidden=FALSE)
    
    
    
    
    
    
  })
  

  
  
}






ANCOM_plot_UI <- function(id){
  ns <- NS(id)
  fluidRow(
    shinyjs::useShinyjs(),
    box(
      width = 12,
      solidHeader = TRUE,
      status = "primary",
      fluidRow(
        #column(12,
              # wellPanel(
                 column(6,
                        uiOutput(ns("ui_select_plot_ids"))
                        ),
                 column(6,
                        numericInput(ns("ncols"),
                                     "Number of columns for combined plot",
                                     min =1, max = 4, step =1,
                                     value = 3
                        )
                 )
               #)
        ##)
      ),
      fluidRow(
        column(12,
               ggplot2_prettier_UI(ns("ANCOM_plot"))
        )
      ),

      fluidRow(
        column(12,
               actionButton(ns("button_plot"),
                            icon = icon("paper-plane"),
                            label = "Plot",
                            style="float:right; color: #fff; background-color: #337ab7; border-color: #2e6da4"
               )
        )
      )
      ,
      fluidRow(

        column(12,
               verbatimTextOutput(ns("console"), placeholder = FALSE)
        )


      )
    ),
    uiOutput(ns("ui_plot"))
    
    )
}

# expression_matrix as long table
ANCOM_plot_SERVER <-function(input, output, session, ancom_out){
  ns <- session$ns
  # preparation
  
  if(class(ancom_out) != "ancom"){
    message("input is not ancom class")
    stop()
  }
  
  
  print(ancom_out$detected)
  # # generate ui for id selection
  # 
  if(length(ancom_out$detected) > 0 && ancom_out$detected != "No significant OTUs detected"){
    output$ui_select_plot_ids <- renderUI({
      "test"
      selectizeInput(ns("select_plot_ids"),
                     "Select to plot",
                     ancom_out$detected,
                     selected =ancom_out$detected[1],
                     multiple = TRUE,
                     options = list(plugins=list('drag_drop','remove_button')
                     )
      )
    })
  }

  # # plotting analysis result
  observe({
    if(input$button_plot > 0 && length(isolate(input$select_plot_ids))){ # isolate to ensure not to plot right away
      #print(input$select_plot_ids)
      p <- isolate({
        plot_ancom_rev(ancom_out,
                       select_plot_ids =  isolate(input$select_plot_ids),
                       ncols =  input$ncols)
        
      })

      p <- callModule(ggplot2_prettier_SERVER, "ANCOM_plot", ggplot2_object = p)

      callModule(plotInBox, "boxplot_ancom",
                 plot_object = p,
                 name_tag = "boxplot_ancom",
                 plot_type = "ggplot2",
                 interactive = FALSE)

      output$ui_plot <- renderUI({

        plotInBox_UI(ns("boxplot_ancom"),
                     boxwidth = 12,
                     plot_height = 600
        )

      })

    }

  })
  
  
  
  
}





ANCOM_UI <- function(id){
  ns <- NS(id)
  fluidRow(
    shinyjs::useShinyjs(),
    box(
      width = 4,
      solidHeader = TRUE,
      status = "primary",
      #wellPanel(
        fluidRow(
          column(6,
                 wellPanel(
                   selectInput(ns("multcorr"),
                               "type of correction for multiple comparisons",
                               c("A stringent correction" = 1, 
                                 "A less stringent correction" =2,
                                 "No correction (default)" =3),
                               selected =3
                   ),
                   numericInput(ns("sig"),
                                "significance level (or FDR)",
                                min =0, max =1, step =0.001,
                                value = 0.05
                   )
                 )
          ),
          column(6,
                wellPanel(
                  tags$strong("Do not change the following unless you know what you are doing"),
                  br(),
                  numericInput(ns("tau"),
                               "tau",
                               min =0, max =1, step =0.001,
                               value = 0.02
                  ),
                  numericInput(ns("theta"),
                               "theta",
                               min =0, max =1, step =0.001,
                               value = 0.1
                  )
                ) 
                
          )
          
        #)
      ),

      
      fluidRow(
        column(12,
               actionButton(ns("button_analysis"), 
                            icon = icon("paper-plane"),
                            label = "Analysis",
                            style="float:right; color: #fff; background-color: #337ab7; border-color: #2e6da4"
               )
        )
      ),
      fluidRow(
        
        column(12,
               verbatimTextOutput(ns("console"), placeholder = FALSE)
        )
        
        
      )
    ),
    #uiOutput(ns("ui_plot"))
    conditionalPanel(
      condition = paste0("output['", ns("analysis_status"), "']"),
      tabBox(
        width = 8,
        tabPanel("Analysis Summary",
                 fluidRow(
                   column(12,
                      uiOutput(ns("ui_analysis_summary"))
                   )
                   
                 )
        ),
        tabPanel("Boxplot",
                 fluidRow(
                   column(12,
                          #uiOutput(ns("ui_plot"))
                          ANCOM_plot_UI(ns("ancom_result_plot"))
                          
                          )
                   
                 )
        ),
        tabPanel("Result Download",
                 fluidRow(
                   column(12,
                          DATATABLE_Display_UI(ns("result_table_view_download"))
                   )
                   
                 )
        )
        
        
      )
      ),
    conditionalPanel(
      condition = paste0("output['", ns("analysis_status_reverse"), "']"),
      box(
        width = 8,
        solidHeader = TRUE,
        status = "primary",
        column(12,
               wellPanel(
                 "OTU or taxon table are",
                 tags$a(href = "https://en.wikipedia.org/wiki/Compositional_data",
                        target = '_blank',
                        "compositional data"
                        )," which needs special treatments",
                 br(),
                 "ANCOM provides a novel method for studying this compositional data. It can be simply used to 
                 do a 'annova'-like test on each single entry to keep the significantly chagned entires.
                 "
                 
               ),
               wellPanel(
                 "Refereneces:",
                 br(),
                 tags$strong("Analysis of composition of microbiomes: a novel method for studying microbial composition."),
                 br(),
                 "https://www.ncbi.nlm.nih.gov/pubmed/26028277"
                 
               )
               )
        
    )
    )

    
  )
}

# expression_matrix as long table
ANCOM_SERVER <-function(input, output, session, expression_matrix, meta_factor){
  ns <- session$ns
  # preparation
  # expression_matrix passed in is a long table
  
  if(length(meta_factor) !=ncol(expression_matrix)){
    message("some samples do not exisit in the meta file")
    stop()
  }
  
  expression_matrix_wide <- as.data.frame(t(expression_matrix))
  
  # check the vector order before passing
  expression_matrix_wide$group <- as.vector(meta_factor)
    
  install.packages.auto("ancom.R")
  
  # doing analysis result
  observe({
    if(input$button_analysis > 0){
      ancom.out <- isolate({
        try(
          withCallingHandlers(
            ancom.out <- ANCOM_rev( OTUdat = expression_matrix_wide, 
                                sig = input$sig, 
                                multcorr = input$multcorr,
                                tau = input$tau,
                                theta =  input$theta
                                ),
            message = function(m) {
              shinyjs::html("console", m$message, add = TRUE)
            },
            warning = function(m) {
              shinyjs::html("console", m$message, add = TRUE)
            }
            
          )
        )
        
      })
      
      # output summary files
      output$n_summary <- renderText({ancom.out$n_summary})
      output$detected <- renderText({ ancom.out$detected})
      output$sub_drop <- renderText({ ancom.out$sub_drop[1,1]})
      output$sub_keep <- renderText({ ancom.out$sub_keep[1,1]})


      output$ui_analysis_summary <- renderUI({ 
        tagList(
          h4("Analysis Summary"),
          verbatimTextOutput(ns("n_summary")),
          h4("Entries detected as significantly changed"),
          verbatimTextOutput(ns("detected")),
          h4("Entries Droped before analysis"),
          verbatimTextOutput(ns("sub_drop")),
          h4("Entries kept for analysis"),
          verbatimTextOutput(ns("sub_keep"))

        )
      })
      
      # output plot
       if(length(ancom.out$detected) > 0 && ancom.out$detected != "No significant OTUs detected"){
         #p <- plot_ancom_rev(ancom.out)
         #print(str(ancom.out))
         try(callModule(ANCOM_plot_SERVER, "ancom_result_plot", ancom_out = ancom.out))
       }

      
      
      # output table if there is any
      if(length(ancom.out$detected) >0 && ancom.out$detected != "No significant OTUs detected"){
        df_detected <- expression_matrix[match(ancom.out$detected, rownames(expression_matrix)),,drop =  FALSE]
 
      }else{
        df_detected <- data.frame()
      }
      callModule(DATATABLE_Display,
                 "result_table_view_download",
                 data_table = df_detected,
                 filename_tag = "detected")
      
      # 
      #
      
    }else{
      ancom.out <- NULL
    }
    
    
    #_____ check the analysis status
    # this is the more efficient way to check the analysis staus and to be used in the UI
    output$analysis_status <- reactive({
      return(!is.null(ancom.out))
    })
    outputOptions(output, 'analysis_status', suspendWhenHidden=FALSE)
    
    output$analysis_status_reverse <- reactive({
      return(is.null(ancom.out))
    })
    outputOptions(output, 'analysis_status_reverse', suspendWhenHidden=FALSE)
    
    
    
  })

  

  
}




AlPHA_DIVERSITY_UI <- function(id){
  ns <- NS(id)
  fluidRow(
    shinyjs::useShinyjs(),
    box(
      width = 4,
      solidHeader = TRUE,
      status = "primary",
      wellPanel(
        fluidRow(
          column(6,
                 selectizeInput(ns("measure_type"),
                                "Select meaure type to plot",
                                c("Observed", "Chao1", "ACE", "Shannon", "Simpson", "InvSimpson", "Fisher"),
                                selected =c("Chao1","Shannon", "Simpson","InvSimpson"),
                                multiple = TRUE, 
                                options = list(plugins=list('drag_drop','remove_button')
                                )
                                ),
                 numericInput(ns("point_size"),
                              "Poit size",
                              min =1, max =10, step =1,
                              value = 5
                              ),
                 sliderInput(ns("nrow"),
                             "Number of Rows",
                             min = 1, max = 3, step = 1,
                             value = 2
                 )
                 
                 
          ),
          column(6,
                 uiOutput(ns("UI_group_selection"))
          )
          
        )
      ),
      fluidRow(
        column(12,
               ggplot2_prettier_UI(ns("ADIVERSITY"))
        )
      ),
      
      fluidRow(
        column(12,
               actionButton(ns("button_analysis"), 
                            icon = icon("paper-plane"),
                            label = "Plot",
                            style="float:right; color: #fff; background-color: #337ab7; border-color: #2e6da4"
               )
        )
      ),
      fluidRow(
        
        column(12,
               verbatimTextOutput(ns("console"), placeholder = FALSE)
        )
        
        
      )
    ),
    uiOutput(ns("ui_plot"))
    # tabBox(
    #   width = 8,
    #   tabPanel("Bar plot",
    #            fluidRow(
    #            uiOutput(ns("ui_plot"))
    #            )
    #   )
    # )
    
  )
}

# expression_matrix as long table
AlPHA_DIVERSITY_SERVER <-function(input, output, session, expression_matrix, meta_df){
  ns <- session$ns
  # expression_matrix passed in is a long table
  
  install.packages.auto(phyloseq)
  
  
  # convert the expression matrix into otu_table class
  taxon_expression_class = otu_table(expression_matrix, taxa_are_rows = TRUE)
  my_phylo <- phyloseq(taxon_expression_class)

  
  # if sample meta data.frame provided, build a sample_data class, and 
  if(!is.null(meta_df)){
     meta_class <- sample_data(meta_df)
     my_phylo <- phyloseq(taxon_expression_class, meta_class)
     
     output$UI_group_selection <- renderUI({
       tagList(
         selectInput(ns("group_selection"),
                     "Choose X to plot",
                     c("samples", colnames(meta_df))
         ),
         selectInput(ns("color_selection"),
                     "Choose color mapping",
                     c("NULL","samples", colnames(meta_df))
         ),
         selectInput(ns("shape_selection"),
                     "Choose shape mapping",
                     c("NULL","samples", colnames(meta_df))
         )
         
       )
       
       
     })
     
     
  }
  
  
  # generate ui for grouping selection


  

  # doing analysis result
  observe({
    if(input$button_analysis > 0){
      p <- isolate({
        try(
          withCallingHandlers(
            plot_richness(my_phylo,
                          x = input$group_selection,
                          title = "Alpha Diversity",
                          color = input$color_selection,
                          shape =input$shape_selection,
                          measures = input$measure_type,
                          nrow = input$nrow
                          ),
            message = function(m) {
              shinyjs::html("console", m$message, add = TRUE)
            },
            warning = function(m) {
              shinyjs::html("console", m$message, add = TRUE)
            }
            
          )
        )
        
      })
      
      
      p <- p + geom_point(size= input$point_size)
      
      p <- callModule(ggplot2_prettier_SERVER, "ADIVERSITY", ggplot2_object = p)
      
      callModule(plotInBox, "scatter_plot",
                 plot_object = p,
                 name_tag = "scatter_plot",
                 plot_type = "ggplot2",
                 interactive = FALSE)
      
      output$ui_plot <- renderUI({
        
        plotInBox_UI(ns("scatter_plot"), 
                     boxwidth = 8,
                     plot_height = 600
        )
        
      })
 
    }

  })
  

}






ggplot2_prettier_UI <- function(id){
  ns <- NS(id)
  install.packages.auto(shinyBS)
  tagList(
    fluidRow(
      column(12,
             
             actionButton(paste0(id,"button_pop"), "More general plot options"), #bsModal has a bug that it does not support namespace, use past0 instead
             bsModal(ns("modalExample"), 
                     "Plot Pretty Opptions", 
                     paste0(id,"button_pop"),  #bsModal has a bug that it does not support namespace, use past0 instead
                     size = "large",
                     box(
                       title = "Frequent used options",
                       width = 12,
                       solidHeader = TRUE,
                       status = "primary",
                       collapsible = TRUE,
                       collapsed = FALSE,
                       column(4,
                              textInput(ns("xy_plot_maintitle"), 
                                        "Main title", 
                                        value = NULL),
                              textInput(ns("xy_plot_xlabel"), 
                                        "X label", 
                                        value = NULL),
                              textInput(ns("xy_plot_ylabel"), 
                                        "Y label", 
                                        value = NULL)
                       ),
                       column(4,
                              numericInput(ns("axis.text.angle.x"),
                                           "X axis text angle",
                                           min = -180, max = 180, step = 1,
                                           value = 0),
                              
                              
                              numericInput(ns("axis.text.angle.y"),
                                           "Y axis text angle",
                                           min = -180, max = 180, step = 1,
                                           value = 0
                              )
                              
                              
                       ),
                       column(4,
                              
                              selectInput(ns("theme"),
                                             "apply plot themes",
                                             c("classic","grey", "blank+white", "light grid", "dark", "minimal", "void", # built in 
                                               "LibreOffice", "Economist magazine","Microsoft Excel","Google Docs", "The Wall Street Journal" # ggthmes
                                               ),
                                             selected = "blank+white"   
                                          ),
                              # selectInput(ns("color_theme"),
                              #             "apply color themes",
                              #             c("classic","LibreOffice","Colorblind safe","The Economist magazine", "Google Docs", "high chart", "Solarized"),
                              #             selected =  "classic" 
                              #             ),
                              
                              numericInput(ns("base_font_size"),
                                           "Base font size",
                                            min = 1, max = 20, step = 1,
                                           value = 11
                                           ),
                              selectInput(ns("base_font_family"),
                                          "Base font family",
                                          c("serif", "sans", "Arial","Arial Black","Times"),
                                          selected = "sans"
                              )
                                        
                       )
                     ),
                    box(
                      title = "Advanced options",
                      width = 12,
                      solidHeader = TRUE,
                      status = "primary",
                      collapsible = TRUE,
                      collapsed = TRUE,
                      column(4,
                             wellPanel(
                               checkboxInput(ns("vertical"),
                                             "Flip over(transpose) plot",
                                             FALSE
                               ),
                               checkboxInput(ns("reverse.x"),
                                             "Reverse X scale",
                                             FALSE
                               ),
                               checkboxInput(ns("reverse.y"),
                                             "Reverse Y scale",
                                             FALSE
                               )
                             )
                             
      
                             
                             ),
                      column(4,
                             wellPanel(
                               selectInput(ns("legend.position"),
                                           "Legend Position",
                                           c("none", "left", "right", "bottom", "top"),
                                           selected = "right"
                                           ),
                               selectInput(ns("legend.direction"),
                                           "Legend direction",
                                           c("horizontal","vertical"),
                                           selected = "vertical"
                               )
                             )
                             
                             ),
                      column(4,
                             wellPanel(
                               "Let me know what you want to customerize",
                               br(),
                               "ningzhibin_at_gmail.com"
                             )
                             )
                     )
             )
      )
    )
  )

}

ggplot2_prettier_SERVER <- function(input, output, session, ggplot2_object){
  ns <- session$ns
  install.packages.auto(ggthemes)
  install.packages.auto(shinyBS)
  
  p_pretty <- ggplot2_object
  
  if(input$reverse.x){
    p_pretty <- p_pretty+scale_x_reverse() 
  }
  
  if(input$reverse.y){
    p_pretty <- p_pretty+scale_y_reverse() 
  }
  
  if(input$vertical){
    p_pretty <- p_pretty+ coord_flip()
  }
  
  # apply built in themes
  switch(input$theme,
         "classic" = {p_pretty <- p_pretty +theme_classic(base_size = input$base_font_size, base_family = input$base_font_family)},
         "blank+white" = {p_pretty <- p_pretty +theme_bw(base_size = input$base_font_size, base_family = input$base_font_family)}, 
         "grey" = {p_pretty <- p_pretty +theme_grey(base_size = input$base_font_size, base_family = input$base_font_family)}, 
         "light grid" = {p_pretty <- p_pretty +theme_light(base_size = input$base_font_size, base_family = input$base_font_family)}, 
         "dark" = {p_pretty <- p_pretty +theme_dark(base_size = input$base_font_size, base_family = input$base_font_family)}, 
         "minimal" = {p_pretty <- p_pretty +theme_minimal(base_size = input$base_font_size, base_family = input$base_font_family)}, 
         "void" = {p_pretty <- p_pretty +theme_void(base_size = input$base_font_size, base_family = input$base_font_family)},
         "LibreOffice" = {p_pretty <- p_pretty +theme_calc(base_size = input$base_font_size, base_family = input$base_font_family)}, 
         "Economist magazine"= {p_pretty <- p_pretty +theme_economist(base_size = input$base_font_size, base_family = input$base_font_family)},
         "Microsoft Excel" = {p_pretty <- p_pretty +theme_excel(base_size = input$base_font_size, base_family = input$base_font_family)},
         "Google Docs" = {p_pretty <- p_pretty +theme_gdocs(base_size = input$base_font_size, base_family = input$base_font_family)}, 
         "The Wall Street Journal "= {p_pretty <- p_pretty + theme_wsj(base_size = input$base_font_size, base_family = input$base_font_family)}
         )
  
  # apply ggthemes for color
  # switch(input$color_theme,
  #        "classic" = {p_pretty <- p_pretty + scale_shape_cleveland()},
  #        "LibreOffice"= {p_pretty <- p_pretty + scale_colour_calc()},
  #        "Colorblind safe" = {p_pretty <- p_pretty + scale_colour_colorblind()},
  #        "The Economist magazine"= {p_pretty <- p_pretty + scale_colour_economist()}, 
  #        "Google Docs"= {p_pretty <- p_pretty + scale_colour_gdocs()}, 
  #        "high chart"= {p_pretty <- p_pretty + scale_colour_hc()}, 
  #        "Solarized"= {p_pretty <- p_pretty + scale_colour_solarized()}
  # )

  # add title and x, y labeling
  if(!is.null(input$xy_plot_maintitle)){
    if(input$xy_plot_maintitle != ""){
      p_pretty <- p_pretty + ggtitle(input$xy_plot_maintitle)
    }
  }
  
  if(!is.null(input$xy_plot_xlabel)){
    if(input$xy_plot_xlabel != ""){
      p_pretty <- p_pretty +xlab(input$xy_plot_xlabel)
    }
    
  }
  
  if(!is.null(input$xy_plot_ylabel)){
    if(input$xy_plot_ylabel != ""){
      p_pretty <- p_pretty +ylab(input$xy_plot_ylabel)
    }
    
  }
  
  # adjust xy text rotation
  p_pretty <- p_pretty +  
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(angle = input$axis.text.angle.x, hjust = 1),
          axis.text.y = element_text(angle = input$axis.text.angle.y, hjust = 1)
        ) 

  # adjust legend postion and direction
  p_pretty <- p_pretty +
    theme(legend.position = input$legend.position,legend.direction = input$legend.direction )
  
  # retrun adjusted ggplot2 
  p_pretty
}




TAXON_COMPOSITION_UI <- function(id){
  ns <- NS(id)
  fluidRow(
    shinyjs::useShinyjs(),
    box(
      width = 4,
      solidHeader = TRUE,
      status = "primary",
      wellPanel(
        fluidRow(
          column(6,
                 checkboxInput(ns("plot_NA"),
                               "Plot black nodes (as one NA category)?",
                               FALSE
                               ),
                 checkboxInput(ns("use_percentage"),
                               "Use percentage(Normalize to 1)",
                               TRUE
                 ),
                 checkboxInput(ns("draw_connection_line"),
                               "If draw connecting line)",
                               FALSE
                 ),
                 uiOutput(ns("ui_level_selection"))
                
          ),
          column(6,
                 sliderInput(ns("BarWidth"),
                             "Bar width",
                             min = 0, max = 1, step = 0.1,
                             value = 0.5
                 ),
                 sliderInput(ns("line_size"),
                             "Connecting line width",
                             min = 0.1, max = 2, step = 0.1,
                             value = 0.5
                 )
                 
                 
                 
                 
                 )
          
        )
      ),
      fluidRow(
        column(12,
               ggplot2_prettier_UI(ns("taxon_composition"))
               )
               ),
      
      fluidRow(
        column(12,
               actionButton(ns("button_analysis"), 
                            icon = icon("paper-plane"),
                            label = "Analyze",
                            style="float:right; color: #fff; background-color: #337ab7; border-color: #2e6da4"
               )
        )
      ),
      fluidRow(
        
        column(12,
               verbatimTextOutput(ns("console"), placeholder = FALSE)
        )
        
        
      )
    ),
    uiOutput(ns("ui_plot"))
    # tabBox(
    #   width = 8,
    #   tabPanel("Bar plot",
    #            fluidRow(
    #            uiOutput(ns("ui_plot"))
    #            )
    #   )
    # )
    
  )
}

# expression_matrix as long table
TAXON_COMPOSITION_SERVER <-function(input, output, session, expression_matrix, lineage_table){
  ns <- session$ns
  # expression_matrix passed in is a long table
  
  
  # prepare uis
  output$ui_level_selection <- renderUI({
    selectInput(ns("level"),
                "Select level to plot",
                colnames(lineage_table)
                )
    
  })
  
  
  
  # doing analysis result
  observe({
    if(input$button_analysis > 0){
      p <- isolate({
        try(
          withCallingHandlers(
            taxon_barplot_by_level(df_taxon_expression = expression_matrix,
                                   df_taxon_lineage = lineage_table,
                                   plot_NA = input$plot_NA,
                                   level = input$level,
                                   use_percentage =  input$use_percentage,
                                   draw_connection_line =  input$draw_connection_line,
                                   BarWidth = input$BarWidth,
                                   line_size =  input$line_size
                                   ),
            message = function(m) {
              shinyjs::html("console", m$message, add = TRUE)
            },
            warning = function(m) {
              shinyjs::html("console", m$message, add = TRUE)
            }
            
          )
        )
        
      })
      
      
      p <- callModule(ggplot2_prettier_SERVER, "taxon_composition", ggplot2_object = p)
      
      callModule(plotInBox, "bar_plot",
                 plot_object = p,
                 name_tag = "bar_plot",
                 plot_type = "ggplot2",
                 interactive = FALSE)
      
      output$ui_plot <- renderUI({
        
        plotInBox_UI(ns("bar_plot"), 
                     boxwidth = 8,
                     plot_height = 600
        )
        
      })
      
      
      
    }
    

    
    
  })
  
  

  
  
}





# expression_matrix as long table
PLSDA_UI <- function(id){
  ns <- NS(id)
  fluidRow(
    shinyjs::useShinyjs(),
    box(
      width = 4,
      solidHeader = TRUE,
      status = "primary",
      wellPanel(
        fluidRow(
          column(12,
                 uiOutput(ns("ui_ncomp")),
                 
                 checkboxInput(ns("if_scale"),
                               "If scale the SD to 1",
                               value =  TRUE),
                 selectInput(ns("mode"),
                             "Algorithm to use",
                             c("regression", "canonical", "invariant","classic")
                             ),
                 numericInput(ns("max.iter"),
                              "Maximum number of iterations",
                              min = 10,
                              max = 1000,
                              value = 100
                              )
                 
                 
          )
          
          
        )
      ),
      
      fluidRow(
        column(12,
               actionButton(ns("button_analysis"), 
                            icon = icon("paper-plane"),
                            label = "Analyze",
                            style="float:right; color: #fff; background-color: #337ab7; border-color: #2e6da4"
               )
        )
      ),
      fluidRow(
        
        column(12,
               verbatimTextOutput(ns("console"), placeholder = FALSE)
        )
      )
    ),
    
    conditionalPanel(
      condition = paste0("output['", ns("analysis_status"), "']"),
      tabBox(
        width = 8,
        
        tabPanel("Analysis Result",
                 fluidRow(
                   #uiOutput(ns("ui_scatter_plot"))
                   column(12,
                          
                          radioButtons(ns("plotting_node"), # using radio button is easy for conditinoal control and allows more option
                                       NULL,
                                       c("Quick scatter plot" = 1, "Advanced scatter plot" = 2),
                                       selected  = 1
                          ),
                          conditionalPanel(
                            condition = paste0("input['", ns("plotting_node"), "']", " == '1' "),
                            plotInBox_UI(ns("scatter_plot"),
                                         boxwidth = 12,
                                         plot_height = 600
                            )
                          ),
                          conditionalPanel(
                            condition = paste0("input['", ns("plotting_node"), "']", " == '2' "),
                            repel_scatter_UI(ns("scatter_plot_advanced"),
                                             group_coloring = TRUE,
                                             group_shaping = TRUE,
                                             collapsed = FALSE
                            )
                          )
                          
                   )
                   
                 )
        ),
        tabPanel("AUC/ROC ",
                 fluidRow(
                   wellPanel(
                     "Note that ROC curve here is from using the same dataset, instead of using new dataset for training",
                     " Therefore it is most likely to be over fitted",
                     "It can only be used as a reference to evaluate the model."
                     
                   ),
                   plotInBox_UI(ns("roc_plot"),
                                boxwidth = 12,
                                plot_height = 600
                   )
                 )
        ),
         tabPanel("Variable Importance in the Projection(VIP) ",
                 fluidRow(
                   plotInBox_UI(ns("vip_plot"),
                                boxwidth = 12,
                                plot_height = 600
                   )
                 )
        ),

        tabPanel("Result Table Download",
                 
                 fluidRow(
                   column(3,
                          radioButtons(ns("analysis_result_download"), 
                                       "Choose result table to view/download",
                                       c("Rotated data matrix " =1,
                                         "Rotation/loadings" = 2, 
                                         "VIP scores" = 3
                                       )
                          )
                   ),
                   column(9,
                          DATATABLE_Display_UI(ns("result_table_view_download"))
                   )
                   
                 )
        )
        
      )
    ),
     conditionalPanel(
       condition = paste0("output['", ns("analysis_status_reverse"), "']"),
                      box(
                        width = 8,
                        solidHeader = TRUE,
                        status = "primary",
                        column(12,
                               wellPanel(
                                 "Here we use PLS-DA as a dimention reduction and feature selection method",
                                 br(),
                                 "PLS-DA can be thought of as a 'supervised'  version of Principal Component Analysis (PCA) in the sense that it 
                                 achieves dimensionality reduction but with full awareness of the class labels ",
                                 br(),
                                 "Besides its use as for dimensionality-reduction, it can be adapted to be used for feature selection [5] as well as for
                                 classification",
                                 br(),
                                 "Since PLS-DA is prone to the problem of overfitting, cross-validation is an important step in using
                                 PLS-DA as a feature selector and a classifier"
                               ),
                               wellPanel(
                                 "Refereneces:",
                                 br(),
                                 "http://mixomics.org/methods/pls-da/"
        
                               )
                               )
                        
                        )
                      )
                     
  )
}




PLSDA_SERVER <-function(input, output, session, expression_matrix, group_factor){
  ns <- session$ns
  install.packages.auto(mixOmics)
  # expression_matrix passed in is a long table
  # extract usefull information
  expression_matrix <- t(expression_matrix)
  number_of_samples <- nrow(expression_matrix)
  number_of_features <- ncol(expression_matrix)
  
  
  try(
    withCallingHandlers(
      (if(length(group_factor) != number_of_samples){
        message("Unequal numbers of grouping factor and samples")
        message(paste0("Sample Number: ", number_of_samples))
        message(paste0("group factor Number: ", length(group_factor) ))
        stop()
      }),
      #can use "warning" instead/on top of "message" to catch warnings too
      message = function(m) {
        shinyjs::html("console", m$message, add = TRUE)
      }

    )
  )

  
  
  # generate some UIs
  output$ui_ncomp <- renderUI({
    
    numericInput(ns("ncomp"),
                "Number of components",
                min = 2, 
                max = min(number_of_samples,number_of_features),
                step = 1,
                value = 2
    )
    
  })
  
  
  # doing analysis result
  analysis_result <- reactive({
    if(input$button_analysis > 0){
      isolate({
        try(
          withCallingHandlers(
           mixOmics::plsda(X = expression_matrix,
                           Y = group_factor, 
                           ncomp = input$ncomp,
                          scale = input$if_scale,
                          mode = input$mode,
                          tol = 1e-06,
                          max.iter = input$max.iter,
                          near.zero.var = FALSE,
                          logratio="none",  # one of "none", "CLR"
                          multilevel=NULL
            )
            ,
            # can use "warning" instead/on top of "message" to catch warnings too
            message = function(m) {
              shinyjs::html("console", m$message, add = TRUE)
            }
            
          )
        )
        
      })
    }
  })
  
  #_____ check the analysis status
  output$analysis_status <- reactive({
    return(!is.null(analysis_result()))
  })
  outputOptions(output, 'analysis_status', suspendWhenHidden=FALSE)
  
  output$analysis_status_reverse <- reactive({
    return(is.null(analysis_result()))
  })
  outputOptions(output, 'analysis_status_reverse', suspendWhenHidden=FALSE)
  
  
  
  
  
  
  # output and plotting
  observe({
    
    if(!is.null(analysis_result())){
      
      
      # df_for_scatter <- data.frame(label = group_factor, analysis_result()$x[,1:3])
      # #df_for_scatter <- data.frame(label = group_factor, PCA_result$x[,1:3])
      # 
      # #plot figure
      # scatter_plot <- scatterplot_ggrepel(df_for_scatter,
      #                                     x_index =2,
      #                                     y_index =3,
      #                                     point_shape_index = 1,
      #                                     point_color_index = 1)
      # 
      
      # extract the data for plotting 
      df_for_scatter <- data.frame(group = analysis_result()$Y, 
                                   analysis_result()$variates$X, 
                                   sample = row.names(analysis_result()$variates$X))
      # plot a quick picture
      scatter_plot <- scatterplot_ggrepel(df_for_scatter,
                          x_index =2,
                          y_index =3,
                          point_shape_index = 1,
                          point_color_index = 1)
     
      
      switch(input$plotting_node, 
             "1" = {
               
               callModule(plotInBox, "scatter_plot",
                          plot_object = scatter_plot,
                          name_tag = "scatter_plot",
                          plot_type = "ggplot2",
                          interactive = FALSE)
             },
             
             "2" = {
               
               callModule(repel_scatter, "scatter_plot_advanced",
                          data_frame = df_for_scatter,
                          x_index =2,
                          y_index =3,
                          grouping_index = 1
               )
             }
      )
      
      #### for AUC ROC plot
      #plot an auc curve
      postscript(tempfile())
      dev.control('enable')
      my.plsda.auroc = mixOmics::auroc(analysis_result(), roc.comp = 1)
      plot_AUCROC <- recordPlot()
      dev.off()
      
      callModule(plotInBox, "roc_plot",
                 plot_object = plot_AUCROC,
                 name_tag = "roc_plot",
                 plot_type = "recordPlot",
                 interactive = FALSE)
      
      ### VIP profiling
      ###  # calculate the VIP scores
      my.vip<-vip(analysis_result())
      my.vip.plot <- matrix_ggboxplot(my.vip, xlabel="Component", ylabel = "VIP Score", maintitle = "VIP Score Across Component")
      callModule(plotInBox, "vip_plot",
                 plot_object = my.vip.plot$violinplot,
                 name_tag = "vip_plot",
                 plot_type = "ggplot2",
                 interactive = FALSE)
      
      #display table 
      
      switch(input$analysis_result_download,
             
             "1" =  {
               callModule(DATATABLE_Display,
                          "result_table_view_download",
                          data_table = analysis_result()$variates$X,
                          filename_tag = "Data_rotated")
             },
             
             "2" = {
               callModule(DATATABLE_Display,
                          "result_table_view_download",
                          data_table = analysis_result()$loadings$X,
                          filename_tag = "Data_rotation")
             },
             "3" = {
               callModule(DATATABLE_Display,
                          "result_table_view_download",
                          data_table = my.vip,
                          filename_tag = "VIP_scores")
             }
             
      )
      
      
    }
    
  })
  
  
}


ggbiplot_UI <-function(id, collapsed = FALSE){
  
  ns <- NS(id)
  fluidRow(
    box(
      title = "Advanced options",
      width = 12,
      solidHeader = TRUE,
      status = "primary",
      collapsible = TRUE,
      collapsed = collapsed,
      
      fluidRow(
        column(3,
                 uiOutput(ns("ui_pc_to_plot")), 
                 wellPanel(
                   checkboxInput(ns("show_circle"),
                                 "Show circle?",
                                 TRUE),
                   sliderInput(ns("circle.prob"),
                               "Circle Probability",
                               min =0, max =1, step = 0.01,
                               value = 0.69) 
                 )
               ),
        
        column(3,
               checkboxInput(ns("if_label_samples"),
                             "Label samples?",
                             FALSE),
               sliderInput(ns("label_size"),
                           "Lable Size",
                           min =1, max =10, step = 1,
                           value = 3),
               sliderInput(ns("alpha"),
                           "transparency",
                           min =0, max =1, step = 0.1,
                           value = 0.7)
        ),
        
        column(3,
               checkboxInput(ns("var.axes"),
                             "Label variables?",
                             TRUE),
               sliderInput(ns("varname.size"),
                           "Variable label size",
                           min =1, max =10, step = 1,
                           value = 3),
               sliderInput(ns("varname.adjust"),
                           "Distance of the variable label to arrows",
                           min =1, max =10, step = 1,
                           value = 1.5)
        ),
        
        column(3,
               wellPanel(
                 uiOutput(ns("ui_grouping"))
               ),
               sliderInput(ns("scale"),
                           "Scale",
                           min =0, max =1, step = 0.1,
                           value = 0.5),
               checkboxInput(ns("plot_interactive"),
                             "Plot interactilvely",
                             FALSE)
        )
      )
      
    ),
    fluidRow(
      column(12,
             column(6,
                    verbatimTextOutput(ns("console"), placeholder = FALSE)
                    
             ),
             column(6,
                    actionButton(ns("button_plot"), 
                                 icon = icon("paper-plane"),
                                 label = "Plot",
                                 style="float:right; color: #fff; background-color: #337ab7; border-color: #2e6da4"
                    )
                    
             )
      )
      
    ),
    br(),
    uiOutput(ns("ui_plot"))
  )
  
}

ggbiplot_SERVER <- function(input, output, session, pcobj, group_factor = NULL){
  ns <- session$ns
  
  install.packages.auto(ggbiplot) # using the ggbiplot in ggbiplot, instead of ggbiplot in ggfortify
  
  
  # extract useful information
 
  number_PCs <- ncol(pcobj$rotation)
  feature_names <- rownames(pcobj$rotation)
  sample_names <- rownames(pcobj$x)
  
  # dynamic ui generation
  
  output$ui_pc_to_plot <- renderUI({
    selectizeInput(ns("choices"),
                   "Choose any 2 components to plot",
                   1:number_PCs ,
                   selected =  1:2,
                   multiple = TRUE, 
                   options = list(maxItems = 2, plugins=list('drag_drop','remove_button'))
    )
    
  })
  
  if(!is.null(group_factor)){
    
    output$ui_grouping <- renderUI({
      tagList(
        checkboxInput(ns("show_ellipse"),
                      "Show ellipse?",
                      FALSE),
        sliderInput(ns("ellipse_prob"),
                    "Ellipse probability",
                    min =0, max =1, step = 0.01,
                    value = 0.68)
      )

    })
  }
  
  observe({
    if (input$button_plot > 0){
      isolate({
        
        if(input$if_label_samples){
          observation_labeling <- sample_names
        }else{
          observation_labeling <- NULL
        }
        

        
        p <- try(
          withCallingHandlers(
            ggbiplot(pcobj, 
                     choices = as.numeric(isolate(input$choices)),
                     scale = input$scale,
                     groups = group_factor,
                     ellipse = input$show_ellipse,
                     ellipse.prob = input$ellipse_prob,
                     labels = observation_labeling,
                     labels.size = input$label_size,
                     alpha =  input$alpha,
                     circle = input$show_circle,
                     circle.prob =input$circle.prob,
                     var.axes = input$var.axes,
                     varname.size = input$varname.size,
                     varname.adjust = input$varname.adjust
            ),
            # can use "warning" instead/on top of "message" to catch warnings too
            message = function(m) {
              shinyjs::html("console", m$message, add = TRUE)
            }
            
          )
        )
        
        
        # prettier
        p <- ggplot2_prettier(p, maintitle = "Biplot")
        
        callModule(plotInBox, "biplot",
                   plot_object = p,
                   name_tag = "biplot",
                   plot_type = "ggplot2",
                   interactive = input$plot_interactive)
        
        
        output$ui_plot <- renderUI({
          
          plotInBox_UI(ns("biplot"), 
                       boxwidth = 12,
                       plot_height = 600
          )
          
        })
      })
    }
  })
  

}




PCA_UI <- function(id){
  ns <- NS(id)
  fluidRow(
    shinyjs::useShinyjs(),
    box(
      width = 4,
      solidHeader = TRUE,
      status = "primary",
       wellPanel(
         fluidRow(
           column(12,
                  "Note that the core function behind IS prcomp, NOT princomp",
                  "'princomp' can only be used with more sampless than variables/features"
                  )
           
        )
       ),
      
      wellPanel(
        fluidRow(
          column(12,
                 checkboxInput(ns("if_center"),
                               "If center (the mean) to zero",
                               value =  TRUE
                 ),
                 checkboxInput(ns("if_scale"),
                               "If scale the SD to 1",
                               value =  TRUE
                 )
          )
          
          
        )
      ),
      
      fluidRow(
        column(12,
               actionButton(ns("button_analysis"), 
                            icon = icon("paper-plane"),
                            label = "Analyze",
                            style="float:right; color: #fff; background-color: #337ab7; border-color: #2e6da4"
               )
        )
      ),
      fluidRow(
        
        column(12,
               verbatimTextOutput(ns("console"), placeholder = FALSE)
        )
        
        
      )
    ),
    
    conditionalPanel(
      condition = paste0("output['", ns("analysis_status"), "']"),
      #condition = paste0("output['", ns("analysis_status"), "']" , "&&","input['", ns("analysis_type"), "']", "=='1'"),
      tabBox(
        width = 8,
        
        tabPanel("PCA clustering Plot",
                 fluidRow(
                   #uiOutput(ns("ui_scatter_plot"))
                   column(12,

                          radioButtons(ns("plotting_node"), # using radio button is easy for conditinoal control and allows more option
                                       NULL,
                                       c("Quick scatter plot" = 1, "Advanced scatter plot" = 2),
                                       selected  = 1
                          ),
                          conditionalPanel(
                            condition = paste0("input['", ns("plotting_node"), "']", " == '1' "),
                            plotInBox_UI(ns("scatter_plot"),
                                         boxwidth = 12,
                                         plot_height = 600
                            )
                          ),
                          conditionalPanel(
                            condition = paste0("input['", ns("plotting_node"), "']", " == '2' "),
                            repel_scatter_UI(ns("scatter_plot_advanced"),
                                             group_coloring = TRUE,
                                             group_shaping = TRUE,
                                             collapsed = FALSE
                            )
                          )
                          
                   )
                   
                 )
        ),
        tabPanel("Scree Plot",
                 fluidRow(
                   plotInBox_UI(ns("screeplot"),
                                boxwidth = 12,
                                plot_height = 600
                   )
                  )
                 ),
        tabPanel("BiPlot",
                 fluidRow(
                   radioButtons(ns("biplot_plotting_node"), # using radio button is easy for conditinoal control and allows more option
                                NULL,
                                c("Quick plot" = 1, "Advanced plot" = 2),
                                selected  = 1
                   ),
                   conditionalPanel(
                     condition = paste0("input['", ns("biplot_plotting_node"), "']", " == '1' "),
                     plotInBox_UI(ns("quick_biplot"),
                                  boxwidth = 12,
                                  plot_height = 600
                     )
                   ),
                   conditionalPanel(
                     condition = paste0("input['", ns("biplot_plotting_node"), "']", " == '2' "),
                     ggbiplot_UI(ns("BiPlot_advanced"))
                   )
                 )
        ),
        tabPanel("Result Table Download",
                 
                 fluidRow(
                   column(3,
                            radioButtons(ns("analysis_result_download"), 
                                         "Choose result table to view/download",
                                         c("standard deviations of the principal components" =1,
                                           "Matrix of variable Rotation/loadings" = 2, 
                                           "Rotated data matrix (the same dimentional matrix with new features/variables)" = 3
                                           
                                           
                                         )
                            )
                          
                          
                          
                   ),
                   column(9,
                          DATATABLE_Display_UI(ns("result_table_view_download"))
                   )
                   
                 )
        )
        
      )
    )
    
  )
}

# expression_matrix as long table
PCA_SERVER <-function(input, output, session, expression_matrix, group_factor){
  ns <- session$ns
  install.packages.auto(ggbiplot) # using the ggbiplot in ggbiplot, instead of ggbiplot in ggfortify
  
  # expression_matrix passed in is a long table
  
  # doing analysis result
  analysis_result <- reactive({
    if(input$button_analysis > 0){
      isolate({
        try(
          withCallingHandlers(
            prcomp(t(expression_matrix), 
                   center = input$if_center,
                   scale. = input$if_scale),
            # can use "warning" instead/on top of "message" to catch warnings too
            message = function(m) {
              shinyjs::html("console", m$message, add = TRUE)
            }
            
          )
        )
        
      })
    }
  })
  
  #_____ check the analysis status
  output$analysis_status <- reactive({
    return(!is.null(analysis_result()))
  })
  outputOptions(output, 'analysis_status', suspendWhenHidden=FALSE)
  
  # output and plotting
  observe({
    
    if(!is.null(analysis_result())){
      
        
      df_for_scatter <- data.frame(label = group_factor, analysis_result()$x[,1:3])
      #df_for_scatter <- data.frame(label = group_factor, PCA_result$x[,1:3])
      
      #plot figure
      scatter_plot <- scatterplot_ggrepel(df_for_scatter,
                                          x_index =2,
                                          y_index =3,
                                          point_shape_index = 1,
                                          point_color_index = 1)
      
      switch(input$plotting_node, 
             "1" = {
               
               callModule(plotInBox, "scatter_plot",
                          plot_object = scatter_plot,
                          name_tag = "scatter_plot",
                          plot_type = "ggplot2",
                          interactive = FALSE)
             },
             
             "2" = {
               
               callModule(repel_scatter, "scatter_plot_advanced",
                          data_frame = df_for_scatter,
                          x_index =2,
                          y_index =3,
                          grouping_index = 1
               )
             }
      )
      
      ### screeplot
      screeplot <- PCA_Screeplot_2(analysis_result())$Scree.plot
      callModule(plotInBox, "screeplot",
                 plot_object = screeplot,
                 name_tag = "screeplot",
                 plot_type = "ggplot2",
                 interactive = FALSE)
      
      # biplot
      quick_biplot <- ggplot2_prettier(ggbiplot(analysis_result()))
  
      switch(input$biplot_plotting_node, 
             "1" = {
               
               callModule(plotInBox, "quick_biplot",
                          plot_object = quick_biplot,
                          name_tag = "quick_biplot",
                          plot_type = "ggplot2",
                          interactive = FALSE)
             },
             
             "2" = {
               
               callModule(ggbiplot_SERVER, "BiPlot_advanced",
                          pcobj = analysis_result(),
                          group_factor = group_factor
               )
             }
      )
      
      # 
      #display table 
      
      switch(input$analysis_result_download,
             
             "1" =  {
               
              
               
               callModule(DATATABLE_Display,
                          "result_table_view_download",
                          data_table = data.frame(sample_name =  colnames(expression_matrix),sdev = analysis_result()$sdev),
                          filename_tag = "PCA_sdev")
             },
             
             "2" = {
               callModule(DATATABLE_Display,
                          "result_table_view_download",
                          data_table = analysis_result()$rotation,
                          filename_tag = "PCA_loadings")
             },
             
             "3" = {
               callModule(DATATABLE_Display,
                          "result_table_view_download",
                          data_table =  t(analysis_result()$x),
                          filename_tag = "PCA_matrix_rotated")
             }
             
      )
      
      
    }
    
  })
  
  
}






TSNE_UI <- function(id){
  ns <- NS(id)
  fluidRow(
    shinyjs::useShinyjs(),
    box(
      width = 4,
      solidHeader = TRUE,
      status = "primary",
      
      
      wellPanel(
        fluidRow(
          column(6,
                 uiOutput(ns("ui_dims")),
                 
                 uiOutput(ns("ui_perplexity")),
                 
                 
                 sliderInput(ns("theta"),
                             "Speed/accuracy trade-off",
                             min = 0, max = 1,step = 0.1,
                             value = 0.5
                 ),
                 checkboxInput(ns("check_duplicates"),
                               "Check duplicates",
                               value =  FALSE
                 )
     
          ),
          column(6,
                 
                 checkboxInput(ns("pca"),
                               "Perform initial PCA",
                               value =  TRUE
                 ),
                 numericInput(ns("max_iter"),
                              "Number of iterations",
                              min = 100, max = 5000, step = 1,
                              value= 1000
                 ),
                 numericInput(ns("stop_lying_iter"),
                              "Iteration after which the perplexities are no longer exaggerated",
                              min = 10, max = 10000, step = 1,
                              value= 250
                 )
          )
          
        )
      ),
      
      fluidRow(
        column(12,
               actionButton(ns("button_analysis"), 
                            icon = icon("paper-plane"),
                            label = "Analyze",
                            style="float:right; color: #fff; background-color: #337ab7; border-color: #2e6da4"
               )
        )
      ),
      fluidRow(
        
        column(12,
               verbatimTextOutput(ns("console"), placeholder = FALSE)
        )
        
        
      )
      ),
    
    conditionalPanel(
      condition = paste0("output['", ns("analysis_status"), "']"),
      #condition = paste0("output['", ns("analysis_status"), "']" , "&&","input['", ns("analysis_type"), "']", "=='1'"),
      tabBox(
        width = 8,
        
        tabPanel("Plot",
                 fluidRow(
                   #uiOutput(ns("ui_scatter_plot"))
                   column(12,
                          
                          radioButtons(ns("plotting_node"), # using radio button is easy for conditinoal control and allows more option
                                        NULL,
                                        c("Quick scatter plot" = 1, "Advanced scatter plot" = 2),
                                        selected  = 1
                                        ),
                          
                          conditionalPanel(
                            condition = paste0("input['", ns("plotting_node"), "']", " == '1' "),
                            plotInBox_UI(ns("scatter_plot"),
                                         boxwidth = 12,
                                         plot_height = 600
                            )
                          ),
                          conditionalPanel(
                            condition = paste0("input['", ns("plotting_node"), "']", " == '2' "),
                            repel_scatter_UI(ns("scatter_plot_advanced"),
                                             group_coloring = TRUE,
                                             group_shaping = TRUE,
                                             collapsed = FALSE
                                             )
                          )
                          
                          )
                   
                 )
                 ),
        tabPanel("Table",
                 fluidRow(
                   DATATABLE_Display_UI(ns("result_table_view_download"))
                 )
        )
    
      )
    )
   
  )
}


TSNE <-function(input, output, session, expression_matrix, group_factor){
  ns <- session$ns
  
  library(Rtsne)

  
  output$ui_dims <- renderUI({
    
    sliderInput(ns("output_dimenstion"),
                "Number of dimenstions to output",
                min = 2, 
                max = ncol(expression_matrix),
                step = 1,
                value = 2
    )
    
  })
  
  
  output$ui_perplexity <- renderUI({
    # see this link of more details
    # https://distill.pub/2016/misread-tsne/
    # https://github.com/jkrijthe/Rtsne/blob/master/R/Rtsne.R
    # if (nrow(X) - 1 < 3 * perplexity) { stop("Perplexity is too large.")}
    number_row <- nrow(expression_matrix)
    if(number_row < 91){
      perplexity_value <- floor((number_row-1)/3)
      perplexity_max <- perplexity_value
    }else{
      perplexity_value <- 30
      perplexity_max <- 100
    }

    sliderInput(ns("perplexity"),
                "Perplexity (Usually 5~50 for high dimention data)",
                min = 5,
                max = perplexity_value,
                step = 1,
                value = perplexity_max
    )

  })

  
  
  # doing analysis result
  tsne_out <- reactive({
     if(input$button_analysis > 0){
       isolate({
         set.seed(0) # set.seed here just before the usage, each time, otherwise, not prproducible
         try(
           withCallingHandlers(
             Rtsne(expression_matrix,
                   dims = input$output_dimenstion,
                   perplexity = input$perplexity,
                   theta = input$theta,
                   check_duplicates = input$check_duplicates,
                   pca = input$pca,
                   max_iter = input$max_iter,
                   stop_lying_iter = input$stop_lying_iter
                   ),
             # can use "warning" instead/on top of "message" to catch warnings too
             message = function(m) {
               shinyjs::html("console", m$message, add = TRUE)
             }
             
           )
         )

         })
     }
  })
  
  #_____ check the analysis status
  output$analysis_status <- reactive({
    return(!is.null(tsne_out()))
  })
  outputOptions(output, 'analysis_status', suspendWhenHidden=FALSE)
  
  
  
  # output and plotting
  observe({
    
    if(!is.null(tsne_out())){
      tsne_df <- data.frame(label = group_factor, tsne_out()$Y)
      
      #plot figure
      scatter_plot <- scatterplot_ggrepel(tsne_df,
                                          x_index =2,
                                          y_index =3,
                                          point_shape_index = 1,
                                          point_color_index = 1)

      switch(input$plotting_node, 
             "1" = {
               
               callModule(plotInBox, "scatter_plot",
                          plot_object = scatter_plot,
                          name_tag = "scatter_plot",
                          plot_type = "ggplot2",
                          interactive = FALSE)
             },
             
             "2" = {
               
               callModule(repel_scatter, "scatter_plot_advanced",
                          data_frame = tsne_df,
                          x_index =2,
                          y_index =3,
                          grouping_index = 1
               )
             }
             )
      
      
      #display table 
      
      
      callModule(DATATABLE_Display,
                 "result_table_view_download",
                 data_table = tsne_out()$Y,
                 filename_tag = "tsne_components")
       
      
      
      
    }

  })

}



repel_scatter_UI <- function(id,
                             layout = "2column",
                             group_coloring = FALSE,
                             group_shaping = FALSE,
                             collapsed = FALSE
){
  ns <- NS(id)
  install.packages.auto(colourpicker)
  
  switch(layout,
         "1column" = {setting_panel_width <- 12
         },
         "2column" = {setting_panel_width <- 5
         }
  )
  
  fluidRow(
    column(
      setting_panel_width,
      box(
        #title = "2D scatter plot",
        width = 12,
        solidHeader = TRUE,
        status = "primary",
        collapsible = TRUE,
        collapsed = collapsed,
        
        fluidRow(
          column(6,
                 # xy mapping
                 uiOutput(ns("ui_x_index")), 
                 uiOutput(ns("ui_y_index")),
                 
                 # for labeling text 
                 wellPanel(
                   checkboxInput(ns("if_label_points_on_plot"),
                                 "Label points?",
                                 value = FALSE
                   ),
                   
                   conditionalPanel(condition = paste0("input['", ns("if_label_points_on_plot"), "']"),
                                    uiOutput(ns("ui_labeling_index")),
                                    
                                    ColorGenerator_UI(ns("label_color"),
                                                      caption = "Set label colors",
                                                      #method = "RcolorBrewer",
                                                      method = "DIY colors",
                                                      Rcolorbrewer_schemes = "div",
                                                      RcolorBrewer_theme = "Spectral"
                                    ),
                                    checkboxInput(ns("if_map_color_label"),
                                                  "Mapping label colors?",
                                                  value = group_coloring
                                    ),
                                    conditionalPanel(condition = paste0("input['", ns("if_map_color_label"), "']"),
                                                     uiOutput(ns("ui_label_color_index"))
                                    ),
                                    selectizeInput(ns("map_label_size"),
                                                   "Label text size by",
                                                   c("Fixed", "Map from data"),
                                                   selected = "Fixed") ,
                                    conditionalPanel(condition = paste0("input['", ns("map_label_size"), "']", " == 'Map from data' "),
                                                     uiOutput(ns("ui_label_size_mapping"))
                                    )
                                    ,
                                    conditionalPanel(condition = paste0("input['", ns("map_label_size"), "']", " == 'Fixed' "),
                                                     sliderInput(ns("label_size"),
                                                                 "Label text Size",
                                                                 min =1, max =20, step = 1,
                                                                 value = 4)
                                    )
                   )
                   
                 ),
                 # for overlay polygon
                 wellPanel(
                   checkboxInput(ns("if_overlap_polygon"),
                                 "Overlap polygon by groups?",
                                 value = FALSE),
                   
                   conditionalPanel(condition = paste0("input['", ns("if_overlap_polygon"), "']"),
                                    radioButtons(ns("polygon_type"),
                                                 "Polygon Type:",
                                                 c("Convex" = "convex","Ellipse" = "ellipse"),
                                                 selected = "ellipse"
                                    ),
                                    conditionalPanel(condition = paste0("input['", ns("polygon_type"), "']", " == 'ellipse' "),
                                                     sliderInput(ns("ellipse_confidence_level"),
                                                                 "Ellipse Confidence Level",
                                                                 min = 0.5, max = 1, step = 0.01,
                                                                 value = 0.95
                                                     )
                                    ),
                                    
                                    uiOutput(ns("ui_polygon_mapping")),
                                    sliderInput(ns("Polygon_Transparency"),
                                                "Polygon_Transparency",
                                                min = 0.1, max = 1, value = 0.5
                                    ),
                                    ColorGenerator_UI(ns("polygon_color"),
                                                      caption = "Customize fill colors",
                                                      method = "RcolorBrewer",
                                                      Rcolorbrewer_schemes = "div",
                                                      RcolorBrewer_theme = "Spectral"
                                    )
                   )
                 ),
                 wellPanel(
                   
                   selectizeInput(ns("map_point_size"),
                                  "Point size by",
                                  c("Fixed", "Map from data"),
                                  selected = "Fixed") ,
                   conditionalPanel(condition = paste0("input['", ns("map_point_size"), "']", " == 'Map from data' "),
                                    uiOutput(ns("ui_point_size_mapping"))
                   )
                   ,
                   conditionalPanel(condition = paste0("input['", ns("map_point_size"), "']", " == 'Fixed' "),
                                    sliderInput(ns("point_size"),
                                                "Point Size",
                                                min =1, max =20, step = 1,
                                                value = 4)
                   )
                 )
          ),
          
          column(6,
                 # for point coloring
                 wellPanel(
                   ColorGenerator_UI(ns("point_color"),
                                     caption = "Set point colors",
                                     method = "RcolorBrewer",
                                     Rcolorbrewer_schemes = "div",
                                     RcolorBrewer_theme = "Spectral"
                   ),
                   checkboxInput(ns("if_color_groups"),
                                 "Mapping point colors?",
                                 value = group_coloring
                   ),
                   conditionalPanel(
                     condition = paste0("input['", ns("if_color_groups"), "']"),
                     uiOutput(ns("ui_point_color_index")),
                     radioButtons(ns("point_color_type"), 
                                  "Coloring type:",
                                  choices = c("group","gradient"), 
                                  selected = "group")
                   )
                   
                   
                 ),
                 # for point shape 
                 wellPanel(
                   
                   checkboxInput(ns("if_shape_groups"),
                                 "Map point shapes?",
                                 value = group_shaping
                   ),
                   conditionalPanel(
                     condition = paste0("input['", ns("if_shape_groups"), "']"),
                     uiOutput(ns("ui_point_shape_index"))
                   ),
                   uiOutput(ns("ui_point_shape_manual"))
                 ),
                 wellPanel(
                   sliderInput(ns("point_transparence"),
                               "Point transparency",
                               min =0.1, max =1, step = 0.1,
                               value = 0.5)
                 )
                 
          )
          
          
        ),
        fluidRow(
          column(6,
                 ggplot2_prettier_UI(ns("scatter_repel"))
          ),
          column(6,
                 checkboxInput(ns("plot_interactive"),
                               "Plot interactilvely",
                               FALSE
                 )
          )
          
        ),
        fluidRow(
          column(6,
                 verbatimTextOutput(ns("console"), placeholder = FALSE)
                 
          ),
          column(6,
                 actionButton(ns("button_plot"), 
                              icon = icon("paper-plane"),
                              label = "Plot Scatterplot",
                              style="float:right; color: #fff; background-color: #337ab7; border-color: #2e6da4"
                 )
                 
          )
          
        )
      ),
      
      br(),
      # here is the plot output, which is already in box
      if(layout == "1column"){ # for one colmn layout
        uiOutput(ns("ui_plot"))
      }
    ),
    # here is the plot output, which is already in box
    if(layout == "2column"){ # for two columns layout
      column(7,
             uiOutput(ns("ui_plot"))
      )
    }
    
  )
}

repel_scatter <- function(input, output, session, data_frame, 
                          x_index =1, 
                          y_index =2, 
                          grouping_index =  NULL
){
  ns <- session$ns
  install.packages.auto(ggrepel)
  # rvalues <- reactiveValues()
  
  
  column_index <- 1: ncol(data_frame)
  names(column_index) <-colnames(data_frame)
  
  output$ui_x_index <- renderUI({
    selectInput(ns("x_index"),
                "Map data as X:",
                column_index,
                selected  = x_index
    )
    
  })
  output$ui_y_index <- renderUI({
    selectInput(ns("y_index"),
                "Map data as Y:",
                column_index,
                selected  = y_index
    )
    
  })
  #
  #ui_point_size_mapping
  output$ui_point_size_mapping <- renderUI({
    selectInput(ns("point_size_mapping_index"),
                "Map data as point size:",
                column_index,
                selected  = NULL
    )
    
  })
  
  # #  ui_label_size_mapping
  output$ui_label_size_mapping <- renderUI({
    selectInput(ns("label_size_mapping_index"),
                "Map data as label size:",
                column_index,
                selected  = NULL
    )
  })
  
  #ui_labeling_index
  output$ui_labeling_index <- renderUI({
    selectizeInput(ns("labeling_index"),
                   "Map data as label text",
                   column_index,
                   selected =  grouping_index,
                   multiple = TRUE, 
                   options = list(maxItems = 1)
    )
  })
  
  # ui for labeling colors
  output$ui_label_color_index <- renderUI({
    selectInput(ns("label_color_index"),
                "Map data as label color:",
                column_index,
                selected  = grouping_index
    )
  })
  
  # UI for polygon group mapping
  output$ui_polygon_mapping <- renderUI({
    selectInput(ns("polygon_mapping"),
                "Map data as polygon groups",
                column_index,
                selected  = grouping_index
    )
  })
  
  # UI for point shape mapping 
  output$ui_point_shape_index <- renderUI({
    selectInput(ns("point_shape_index"),
                "Choose data",
                column_index,
                selected  = grouping_index
    )
  })
  
  # UI for point color mapping
  output$ui_point_color_index <- renderUI({
    selectInput(ns("point_color_index"),
                "Group point color by:",
                column_index,
                selected  = grouping_index
    )
  })  
  
  
  ## for the manual point shape 
  pch_list <- c(
    "solid square" = 15, 
    "solid circle" = 16, 
    "solid triangle point-up" = 17, 
    "solid diamond" = 18,
    "square" = 0,
    "circle" = 1,
    "triangle point up" = 2,
    "plus" = 3,
    "cross" = 4,
    "diamond" = 5,
    "triangle point down" = 6,
    "square cross" = 7,
    "star" = 8,
    "diamond plus" = 9,
    "circle plus" = 10,
    "triangles up and down" = 11,
    "square plus" = 12,
    "circle cross" = 13,
    "square and triangle down" = 14
    
    #, 
    ## these symbols are filled with color, do not bother
    # "solid circle" = 19, 
    # "bullet (smaller circle)" = 20,
    # "filled circle" = 21, 
    # "filled square" = 22, 
    # "filled diamond" = 23, 
    # "filled triangle point-up" = 24, 
    # "filled triangle point down" = 25
  )
  
  output$ui_point_shape_manual <- renderUI({
    if(input$if_shape_groups){
      
      n_point_shape <- nlevels(as.factor(data_frame[[as.numeric(input$point_shape_index)]])) 
      selectizeInput(ns("point_shape_manual_scale"),
                     "Set point shape:",
                     pch_list,
                     selected  = pch_list[1:n_point_shape],
                     multiple = TRUE, 
                     options=list(maxItems = n_point_shape,plugins=list('drag_drop','remove_button'))
      )
      
    }else{# if no mapping, only one chareacter
      selectizeInput(ns("point_shape_manual_scale"),
                     "Set point shape:",
                     pch_list,
                     selected  = 16,
                     options=list(plugins=list('drag_drop','remove_button'))
      )
      
    }
    
    
  })
  
  ####
  # prepare the arguments
  
  observe({
    #  for text labeling 
    if(input$if_label_points_on_plot){
      label_index_set <- as.numeric(input$labeling_index)
    }else{
      label_index_set <- NULL
    }
    
    #____________________________________
    # for text label colorining
    
    
    if(input$if_map_color_label){
      if(!is.null(input$label_color_index)){
        label_color_index_set <- as.numeric(input$label_color_index)
        
        n_colors_label <- nlevels(as.factor(data_frame[[label_color_index_set]]))
        mycolors_label_mulitiple <- callModule(ColorGenerator_server, "label_color",
                                               Rcolorbrewer_schemes = "div",
                                               RcolorBrewer_theme = "Spectral",
                                               ncolors = n_colors_label)
        #print(mycolors_label_mulitiple())
        label_color_set <- isolate(mycolors_label_mulitiple()) 
      }
      
    }else{
      # get colors from the module
      mycolors_label_single <- callModule(ColorGenerator_server, "label_color",
                                          #method = "RcolorBrewer",
                                          Rcolorbrewer_schemes = "div",
                                          RcolorBrewer_theme = "Spectral",
                                          ncolors = 1)
      #print(mycolors_label_single())
      label_color_index_set <- NULL
      label_color_set <- isolate(mycolors_label_single()) # set the color using isolate, not reactivate(re evaluate untill used)
    }
    #____________________________________
    
    
    # for point colorining
    if(input$if_color_groups && !is.null(input$point_color_index)){
      point_color_index_set <- as.numeric(input$point_color_index)
      
      n_colors_point <- nlevels(as.factor(data_frame[[point_color_index_set]]))
      
      mycolors_point_mulitiple <- callModule(ColorGenerator_server, "point_color",
                                             Rcolorbrewer_schemes = "div",
                                             RcolorBrewer_theme = "Spectral",
                                             ncolors = n_colors_point)
      
      point_color_set <- isolate(mycolors_point_mulitiple()) 
      
      
    }else{
      # get colors from the module
      mycolors_point_single <- callModule(ColorGenerator_server, "point_color",
                                          Rcolorbrewer_schemes = "div",
                                          RcolorBrewer_theme = "Spectral",
                                          ncolors = 1)
      
      point_color_index_set <- NULL
      point_color_set <- isolate(mycolors_point_single()) # set the color using isolate, not reactivate(re evaluate untill used)
    }
    
    
    
    # for point shape
    if(input$if_shape_groups){
      point_shape_index_set <- as.numeric(input$point_shape_index)
    }else{
      point_shape_index_set <- NULL
    }  
    
    # for point size
    switch(input$map_point_size,
           "Fixed" = { point_size_index_set <- NULL},
           "Map from data" = {
             point_size_index_set <- as.numeric(input$point_size_mapping_index)
           }
    )
    
    # for label text size
    if(!is.null(input$map_label_size)){
      switch(input$map_label_size,
             "Fixed" = { label_size_index_set <- NULL},
             "Map from data" = {
               label_size_index_set <- as.numeric(input$label_size_mapping_index)
             }
      )
    }
    
    
    # for polygon colors
    if(input$if_overlap_polygon){
      
      polygon_index_set <- as.numeric(input$polygon_mapping)
      n_colors_polygon <- nlevels(as.factor(data_frame[[polygon_index_set]]))
      
      # get colors from the module
      mycolors_polygon <- callModule(ColorGenerator_server, "polygon_color",
                                     Rcolorbrewer_schemes = "div",
                                     RcolorBrewer_theme = "Spectral",
                                     ncolors = n_colors_polygon)
      polygon_color_manual_set <- isolate(mycolors_polygon()) # set the colors, with isolate
    }else{
      polygon_index_set <- NULL
      polygon_color_manual_set <- NULL
    }
    
    ###
    # now start plotting
    
    
    if (input$button_plot > 0){
      
      # check the real-time arguments
      #print("_________________________________________________________________")
      #print(paste("Point shape index:", point_shape_index_set))
      #print("Point shapes manual:")
      #print(as.numeric(input$point_shape_manual_scale))
      #print(paste("point_color_index: ", point_color_index_set))
      #print(paste("point_color_type:", input$point_color_type))
      #print(paste("polygon_index: ",polygon_index_set ))
      #print(paste("polygon_color_manual", polygon_color_manual_set))
      
      isolate({
        #df <- data.frame(x = rnorm(10), y = rnorm(10), g=sample(LETTERS[1:3], 10, replace =  TRUE))
        #plot <- ggplot(df, aes(x = x, y = y,shape = g))+geom_point() + scale_shape_manual(values = 12:14)
        this_plot <- try(
          withCallingHandlers(
            #plot,
            advanced_scatterplot(data_frame,
                                 x_index = as.numeric(input$x_index),# column index/location
                                 y_index = as.numeric(input$y_index),# column index/location
                                 
                                 point_shape_index = point_shape_index_set, # column index/location
                                 point_shape_manual = as.numeric(input$point_shape_manual_scale),
                                 
                                 point_color_index = point_color_index_set, # column index/location
                                 point_color_manual = point_color_set, # if point_color_index is null, only use the first color, otherwise
                                 point_color_type = input$point_color_type, # c("graidnet", "group"), if group, will force to factor
                                 
                                 point_size_index = point_size_index_set,
                                 point_size_manual = input$point_size,
                                 
                                 point_alpha = input$point_transparence,
                                 
                                 overlay_polygon = input$if_overlap_polygon,
                                 polygon_type =  input$polygon_type, # or "ellipse"
                                 polygon_index = polygon_index_set, # draw polygon overlap, will choose
                                 polygon_alpha = input$Polygon_Transparency,
                                 ellipse_confidience = input$ellipse_confidence_level, # only works when ellipse is choose for polygon_type
                                 polygon_fill_color_manual = polygon_color_manual_set, # null means auto color, otherwise provide list of color values
                                 
                                 label_text_index = label_index_set,
                                 label_color_index = label_color_index_set,
                                 label_color_manual = label_color_set, # this is to do
                                 label_size_index =  label_size_index_set,
                                 label_size_manual  = input$label_size,
                                 
                                 equal_xy = FALSE
                                 
            ),
            # can use "warning" instead/on top of "message" to catch warnings too
            message = function(m) {
              shinyjs::html("console", m$message, add = TRUE)
            }
            
          )
        )
        
        
        this_plot <- callModule(ggplot2_prettier_SERVER, "scatter_repel", ggplot2_object = this_plot)
        
        callModule(plotInBox, "this_plot",
                   plot_object = this_plot,
                   name_tag = "scatter_plot",
                   plot_type = "ggplot2",
                   interactive = input$plot_interactive)
        
        
        output$ui_plot <- renderUI({
          
          plotInBox_UI(ns("this_plot"), 
                       boxwidth = 12,
                       plot_height = 600
          )
          
        })
        
        
      })
      
      
    }
  })
  
}



# callModule(plotInBox, "mq_summary",
#            plot_object = mq_summary,
#            name_tag = "mq_summary",
#            plot_type = "ggplot2")
# 
# output$ui_plot_output <- renderUI({
#   plotInBox_UI(ns("mq_summary"), boxwidth = 8)
# })





highchart_scatter_UI <- function(id){
  ns <- NS(id)
  
  jqui_resizabled(highchartOutput(ns('hc'), 
                                    width = '400px', 
                                    height = '400px'))
  

  
}

highchart_scatter <- function(input, output, session, data_frame){
  ns <- session$ns
  
  output$hc <- renderHighchart({
    hchart(data_frame, "scatter", hcaes(x = cyl, y = mpg, group = factor(vs))) %>%
      hc_legend(enabled = TRUE)  %>% 
      hc_exporting(enabled = TRUE,filename = "custom-file-name")
  })
}






RedirectConsole_UI <- function(id){
  ns <- NS(id)
  fluidRow(
    shinyjs::useShinyjs(),
    actionButton(ns("apply"), "Start"),
    #pre(id = ns("console"))
    verbatimTextOutput(ns("console"), placeholder = FALSE)
  )
}

RedirectConsole <- function(input, output, session){
  ns <- session$ns
  
  longfunc <- function() {
    message("Thinking...")
    Sys.sleep(1)
    message("Still thinking...")
    Sys.sleep(1)
    message("Done")
  }
  
  
  observeEvent(input$apply, {
    try(
      withCallingHandlers(
      #longfunc(),
      { message("This is a test for message ")
        warning("warning message comes here")
        read.delim("file_upload_test_duplicate_rownames.txt", header=TRUE, sep="\t", 
                   check.names =  TRUE, 
                   row.names = 1)
        },
      
      # can use "warning" instead/on top of "message" to catch warnings too 
      message = function(m) {
        shinyjs::html("console", m$message, TRUE)
      },
      warning = function(m) {
        shinyjs::html("console", m$message, add = TRUE)
      }
    )
  )
  })
}




BATCHEFFECT_UI <- function(id){
  ns <- NS(id)
  fluidRow(
    shinyjs::useShinyjs(),
    box(
      width = 5,
      solidHeader = TRUE,
      status = "primary",
      wellPanel(
          radioButtons(ns("analysis_type"),
                       "Choose analysis type",
                       c("using ComBat only (Very fast for large dataset)"  = "3",
                         "Both comBat and correctBatch (slow for >1000 features)" = "1", 
                         "Explore batch effect and correct it with Both comBat and correctBatch (slow for >1000 features)" = "2"
                       ),
                       selected = 3
          )
      ),
      
      wellPanel(
        fluidRow(
          column(8,
                 uiOutput(ns("ui_select_topNSD")),
                 uiOutput(ns("ui_SD_threshold"))
          )
          ,
          column(4,
                 selectInput(ns("Scale_before_correction"),
                             "Scale data before correction?",
                             c("NO" = "none", "YES" = "unit")
                             
                 ),
                 conditionalPanel(
                   condition = paste0("input['", ns("analysis_type"), "']", "=='2'"),
                   uiOutput(ns("ui_maxdim"))
                 )
                )
          )
        ),
        fluidRow(
          column(8,
                 plotOutput(ns("ui_plot_sd_distribution"),width = "100%", height = "300px")
                 ),
          
          column(4,
                 verbatimTextOutput(ns("console"), placeholder = FALSE)
                 )
          
          
         ),
      
      fluidRow(
        column(6),
        column(6,
               actionButton(ns("button_plot"), 
                            icon = icon("paper-plane"),
                            label = "GO Analysis",
                            style="float:right; color: #fff; background-color: #337ab7; border-color: #2e6da4"
               )
        )
      )
      #,
      #verbatimTextOutput(ns("console"))
      #br(),
      #verbatimTextOutput(ns("console"))
      # remember to use css to format the look
      #pre(id =  ns("console")) # this also works
    ),
    
    #uiOutput(ns("ui_simple_analysis_result_view")),
    conditionalPanel(
      condition = paste0("output['", ns("analysis_status"), "']"),
      #condition = paste0("output['", ns("analysis_status"), "']" , "&&","input['", ns("analysis_type"), "']", "=='1'"),
      tabBox(
        #id = "tabset1",  # The id lets us use input$tabset1 on the server to find the current tab
        width = 7,

        tabPanel("Comparision before and after correction",
                 fluidRow(
                   conditionalPanel(
                     condition = paste0("input['", ns("analysis_type"), "']", "=='2'"),
                     
                     column(10,
                            wellPanel(
                              "For Exploring mode, the method doing more assessment to decide the numbers of pPC first.
                              The batch effect (before and after correction using two method) can be quantified and visualized by the box plot.
                              Download the data from the download tab."
                            )
                     ),
                     plotInBox_UI(ns("Batch_explore"),
                                  boxwidth = 12,
                                  plot_height = 800
                     )
                   ),
                   column(10,
                          wellPanel(
                            "PCA plot is usually used to visual check if there is any obivous batch effects. 
                            Two methods, ComBat and CorrrectBatch (based on probability) are used to do the correction. 
                            You can clearly tell the power of the correction by comparing the PCA plot before and after. 
                            Download the data from the download tab."
                          )
                          ),
                   
                   plotInBox_UI(ns("PCA_compare"), 
                                boxwidth = 12,
                                plot_height = 800
                   )
                 )
        ),
        
        tabPanel("More supporting plots",
                 fluidRow(
                   column(10,
                          wellPanel(
                            "You can also check the proportion of PCA variations accross all PCs from PCA anlaysis. 
                            There is for sure some variation beteen the two method correction, but usually highly consistent to earch other.
                            "
                          )
                          ),
                   plotInBox_UI(ns("PCA_more"), 
                                boxwidth = 12,
                                plot_height = 400
                   ),
                   conditionalPanel(
                     condition = paste0("input['", ns("analysis_type"), "']", "=='2'"),
                     column(10,
                            wellPanel(
                              "The batch effect on each pairs of the pPCs using PPCA analysis for the optimized number of pPCs"
                            )
                            ),
                     plotInBox_UI(ns("pPCA_pairs"),
                                  boxwidth = 12,
                                  plot_height = 800
                     )
                     
                   ),
                   
                   column(10,
                          wellPanel(
                            "The batch effect on each pairs of the PCs using PCA analysis for the top 10 PCs"
                          )
                          ),
                   plotInBox_UI(ns("PCA_pairs"), 
                                boxwidth = 12,
                                plot_height = 800
                   )
                   
                 )
        ),
        tabPanel("Result data for download",
                 fluidRow(
                   column(3,
                          conditionalPanel(
                            condition = paste0("input['", ns("analysis_type"), "']", "=='1'", "||","input['", ns("analysis_type"), "']", "=='2'"),
                            radioButtons(ns("simple_analysis_data_table1"), 
                                         "Choose result table to view/download",
                                         c("PCA plot data for ComBat corrected data",
                                           "PCA plot data for correctBatch corrected data",
                                           "ComBat corrected data",
                                           "correctBatch corrected data",
                                           "Proportion Variation PCA before Correction",
                                           "All PCs Before Correction"
                                         )
                            )
                            ),
                          conditionalPanel(
                            condition = paste0("input['", ns("analysis_type"), "']", "=='3'"),
                            radioButtons(ns("simple_analysis_data_table3"), 
                                         "Choose result table to view/download",
                                         c("PCA plot data for ComBat corrected data",
                                           "ComBat corrected data",
                                           "Proportion Variation PCA before Correction",
                                           "All PCs Before Correction"
                                         )
                            )
                          )
                          
                          
                   ),
                   column(9,
                          DATATABLE_Display_UI(ns("result_table_view_download"))
                   )
                   
                 )
        )
        
      )
    ),
    shinysky::busyIndicator(text = "busy, won't be long ...",
                            img = "ajaxloaderq.gif",
                            wait = 1000)
    
    
    
    
  )
}

BATCHEFFECT <- function(input, output, session, expression_matrix, meta){
  ns <- session$ns
  source("remove_batcheffect_modified_functions.R")
  sds <- future_apply(expression_matrix, 2, sd)
  
  # order the expression_matrix accordint to the SD value
  expression_matrix <- expression_matrix[,order(sds, decreasing = TRUE)]
  #sds_check <- future_apply(pg_ordered, 2, sd)
  
  if(ncol(expression_matrix) < 500){
    topN <- ncol(expression_matrix)
  }else{
    topN <- 500
  }
  
  output$ui_select_topNSD <- renderUI({
    sliderInput(ns("select_topNSD"),
                "Only use topN features with highest SD",
                min = 1,
                max = ncol(expression_matrix),
                value = topN,
                step = 1
                )
  })
  
  rvalues <- reactiveValues()
  
  observe({
    if(!is.null(input$select_topNSD)){
      #print(input$select_topNSD)
      rvalues$expression_matrix <- expression_matrix[,1:input$select_topNSD]
      rvalues$sds <- sort(sds, decreasing =  TRUE)[1:input$select_topNSD]
    }else{
      rvalues$expression_matrix <- expression_matrix
      rvalues$sds <- sds
    }
  })
  
  
  output$ui_plot_sd_distribution <- renderPlot({
    
    ## Calculate and plot the two histograms
    hcum <- h <- hist(rvalues$sds, plot=FALSE)
    hcum$counts <- cumsum(hcum$counts)
    
    # plot the histograme
    plot(1,1, xlim = c(hcum$breaks[1], max(hcum$breaks)), ylim =  c(0,max(hcum$counts)),
         main= "SD cut-off Profile",
         xlab = "SD cut off", 
         ylab = "Numbers of featrues left", type =  "n")
    d <- density(rvalues$sds)
    lines(x = d$x, y = (1-cumsum(d$y)/max(cumsum(d$y))) * length(rvalues$sds), lwd = 2, col = "red")
    
  })
  
  output$ui_SD_threshold  <- renderUI({
    
    sd_range <- range(rvalues$sds)
    sliderInput(ns("SD_threshold"),
                "Filter out features with small SDs (refer to the curve below see the sd cutoff and features left):",
                min = round(sd_range[1], digits = 1),
                max = round(sd_range[2], digits = 1),
                step = 0.02,
                value = sd_range[1]
                )
  })
  
  output$ui_maxdim <-renderUI({
    number_sample <- nrow(rvalues$expression_matrix)
    if(number_sample >10){
      max_dim  <- 10
      
    }else{
      max_dim <- number_sample
    }
    
    
    sliderInput(ns("ui_maxdim"),
                "Set maximum number of dimentions",
                min = 2,
                max = number_sample,
                value = max_dim
                )
    
  })
  
  
  analysis_result <- reactive({
    # note that using this way, will not return a null automatically if error occurred inside the function
    # you have to put a retun(NULL) or using ractive values, and use exists to test before using
  #observe({
    
    if(input$button_plot > 0){
      isolate({
        switch(isolate(input$analysis_type),
               "1" = {
                 try(
                   withCallingHandlers(
                     expBATCH_rev_simple(D = rvalues$expression_matrix,
                                         batchCL = meta,  # grouping information as factors
                                         SDselect = as.numeric(input$SD_threshold),
                                         scale = input$Scale_before_correction),
                     # can use "warning" instead/on top of "message" to catch warnings too
                     message = function(m) {
                       shinyjs::html("console", m$message, add = TRUE)
                     }
                     
                   )
                 )
               },
               "2" = {
                 try(
                   withCallingHandlers(
                     expBATCH_rev(D = expression_matrix,
                                  batchCL = meta,  # grouping information as factors
                                  SDselect = as.numeric(input$SD_threshold),
                                  scale = input$Scale_before_correction,
                                  method = "ppcca",
                                  mindim = 2,
                                  maxdim = 9,
                                  Conf = NA
                     ),
                     # can use "warning" instead/on top of "message" to catch warnings too
                     message = function(m) {
                       shinyjs::html("console", m$message, add = TRUE)
                     }
                     
                   )
                 )
               } ,
               
               "3" = 
               {
                 try(
                   withCallingHandlers(
                     comBat_only(D = rvalues$expression_matrix,
                                 batchCL = meta,  # grouping information as factors
                                 SDselect = as.numeric(input$SD_threshold),
                                 scale = input$Scale_before_correction),
                     # can use "warning" instead/on top of "message" to catch warnings too
                     message = function(m) {
                       shinyjs::html("console", m$message, add = TRUE)
                     }
                     
                   )
                 )
               }
               )
        
      })
    }
    
    
  })
 
  # 
  
  #_____ check the analysis status
  output$analysis_status <- reactive({
    return(!is.null(analysis_result()))
  })
  outputOptions(output, 'analysis_status', suspendWhenHidden=FALSE)
  
  
  #______ for plotting the outcomes
  observe({

        if(!is.null(analysis_result())){
          
           # only if result is generetate, generate the ui
          switch(isolate(input$analysis_type),
            "1" = {combined_plot_PCA <- shiny_multiplot(analysis_result()$result_pcaBeforeCorrection$pcaBeforeCorrection_plot,
                                                        analysis_result()$result_correctComBat$pca_After_Combat_Correction_plot,
                                                        analysis_result()$result_correctBatch$pca_After_PPCCA_correction_plot,
                                                        cols = 2)},
            "2" = {combined_plot_PCA <- shiny_multiplot(analysis_result()$result_ppccaBeforeCorrection$ppccaBeforeCorrection_plot,
                                                        analysis_result()$result_pcaBeforeCorrection$pcaBeforeCorrection_plot,
                                                        analysis_result()$result_correctComBat$pca_After_Combat_Correction_plot,
                                                        analysis_result()$result_correctBatch$pca_After_PPCCA_correction_plot,
                                                        cols = 2)
            
                 },
            "3" = {combined_plot_PCA <- shiny_multiplot(analysis_result()$result_pcaBeforeCorrection$pcaBeforeCorrection_plot,
                                                        analysis_result()$result_correctComBat$pca_After_Combat_Correction_plot,
                                                        cols = 2)}
          )
            
          #}
          
        #   #layout <- matrix(c(1, 1, 2, 3), nrow = 2, byrow = TRUE)
          combined_plot_PCA_more <- shiny_multiplot( analysis_result()$result_pcaBeforeCorrection$ProportionVariationPCAbeforeCorrection_plot,
                                                     analysis_result()$result_correctBatch$ComBat_vs_PPCCA_predictedData_plot,
                                                     cols = 2)
       # call the module to visualize in the browser
          callModule(plotInBox, "PCA_compare",
                     plot_object = combined_plot_PCA,
                     name_tag = "PCA_compare",
                     plot_type = "recordPlot")
        #   
          callModule(plotInBox, "PCA_more",
                     plot_object = combined_plot_PCA_more,
                     name_tag = "PCA_more",
                     plot_type = "recordPlot")
        #   
          callModule(plotInBox, "PCA_pairs",
                     plot_object = analysis_result()$result_pcaBeforeCorrection$pairspcaBeforeCorrection_plot,
                     name_tag = "PCA_pairs",
                     plot_type = "recordPlot")
          
          
          if(isolate(input$analysis_type) == "3" ){

            switch(input$simple_analysis_data_table3,

                   "PCA plot data for ComBat corrected data" =  {
                     callModule(DATATABLE_Display,
                                "result_table_view_download",
                                data_table = analysis_result()$result_correctComBat$pca_After_Combat_Correction,
                                filename_tag = "pca_After_Combat_Correction")
                      },

                   "ComBat corrected data" = {
                     callModule(DATATABLE_Display,
                                "result_table_view_download",
                                 data_table = analysis_result()$result_correctComBat$combatCorrectedData,
                                 filename_tag = "combatCorrectedData")
                     },

                   "Proportion Variation PCA before Correction" = {
                     callModule(DATATABLE_Display,
                                "result_table_view_download",
                                data_table = analysis_result()$result_pcaBeforeCorrection$ProportionVariationPCAbeforeCorrection,
                                 filename_tag = "ProportionVariationPCAbeforeCorrection")
                     },

                   "All PCs Before Correction" = {
                     callModule(DATATABLE_Display,
                                "result_table_view_download",
                                data_table = analysis_result()$result_pcaBeforeCorrection$pairspcaBeforeCorrection,
                                filename_tag = "pairspcaBeforeCorrection")
                     }

            )
          }else {
            switch(input$simple_analysis_data_table1,
                   
                   "PCA plot data for ComBat corrected data" =  {
                     callModule(DATATABLE_Display, 
                                "result_table_view_download",
                                data_table = analysis_result()$result_correctComBat$pca_After_Combat_Correction,
                                filename_tag = "pca_After_Combat_Correction")
                     },
                   "PCA plot data for correctBatch corrected data" = {
                     callModule(DATATABLE_Display, 
                                "result_table_view_download",
                                data_table = analysis_result()$result_correctBatch$pca_After_PPCCA_correction,
                                filename_tag = "pca_After_PPCCA_correction")
                     },
                   
                   "ComBat corrected data" = {
                     callModule(DATATABLE_Display, 
                                "result_table_view_download",
                                data_table = analysis_result()$result_correctComBat$combatCorrectedData,
                                filename_tag = "combatCorrectedData")
                     },
                   "correctBatch corrected data" = {
                     callModule(DATATABLE_Display, 
                                "result_table_view_download",
                                data_table = t(analysis_result()$result_correctBatch$ppccaCorrectedData),
                                filename_tag = "proportion_of_varition_before_correction")
                     },
                   
                   "Proportion Variation PCA before Correction" = {
                     callModule(DATATABLE_Display, 
                                "result_table_view_download",
                                data_table = analysis_result()$result_pcaBeforeCorrection$ProportionVariationPCAbeforeCorrection,
                                filename_tag = "ProportionVariationPCAbeforeCorrection")
                     },
                   
                   "All PCs Before Correction" = {
                     callModule(DATATABLE_Display, 
                                "result_table_view_download",
                                data_table = analysis_result()$result_pcaBeforeCorrection$pairspcaBeforeCorrection,
                                filename_tag = "pairspcaBeforeCorrection")
                     }
                   
            )
          }
          

         
          
          # for figures and output specific for the 2 analysis
          if(isolate(input$analysis_type) == "2"){
            Batch_explore <- shiny_multiplot(analysis_result()$result_ppccaBeforeCorrection$NumberOfpPCs_plot,
                                             analysis_result()$result_findBATCH$batchEffect_plot,
                                             analysis_result()$result_assessComBat$batchEffect_plot,
                                             analysis_result()$result_assessCorrectBATCH$batchEffect_plot,
                                             cols = 2
            ) 
            # call the module to visualize in the browser
            callModule(plotInBox, "Batch_explore",
                       plot_object = Batch_explore,
                       name_tag = "Batch_explore",
                       plot_type = "recordPlot")
            
            
            callModule(plotInBox, "pPCA_pairs",
                       plot_object = analysis_result()$result_ppccaBeforeCorrection$pairsppccaBeforeCorrection_plot,
                       name_tag = "pPCA_pairs",
                       plot_type = "recordPlot")
            
            
          }

          
          
    }
  })
}




#

MQ_SUMMARY_UI <-function(id){
  ns <- NS(id)
  fluidRow(
    box(
      width = 4,
      solidHeader = TRUE,
      status = "primary",
      
      wellPanel(
        fluidRow(
          column(6,
                 uiOutput(ns("ui_select_data"))
                 ,
                 
                 
                 checkboxInput(ns("if_vertical"),
                               "Transpose plot?",
                               value =  FALSE
                               
                 )
                 
                 # checkboxInput(ns("show_highlight_options"),
                 #               "Simple Highlight/group data?",
                 #               value = FALSE
                 #               
                 #               )
                 ),
          column(6,
                 selectInput(ns("plot_type"),
                             "Select plot type",
                             c("scatter", "bar", "density", "histogram", "freqpoly", "box", "violin"),
                             selected = "scatter"
                             ),
                 
                 textInput(ns("Threshold_line_value"),
                           "Set threshold line on the plot",
                           value = "20"
                           
                           )
                 )
            
          ),
        #conditionalPanel(
        #  condition = paste0("input['", ns("show_highlight_options"), "']"),
        # this ui is both for meta data and rows highlighted (if no meta provided)
          uiOutput(ns("ui_select_grouping"))
        #)
        
      ),
      
      fluidRow(
        column(6),
        column(6,
               actionButton(ns("button_plot"), 
                            icon = icon("paper-plane"),
                            label = "Plot",
                            style="float:right; color: #fff; background-color: #337ab7; border-color: #2e6da4"
               )
        )
      )
      
      ),
    
    # here is the plot output, which is already in box
    uiOutput(ns("ui_plot_output"))
  )
  

}


MQ_SUMMARY <- function(input, output, session, data_frame, meta = NULL){
  # data_frame is organized data.frame with first column as rawfile names, and then columns to plot
  # meta data is a data.frame, which should cover all rawfiles in the data_frame 
  # meta structure: first column as rawfile names, then column(s) of grouping information
  
  ns <- session$ns
  # start plotting
  row.names(data_frame) <- data_frame[,1]
  # generate ui for column selection
  output$ui_select_data <- renderUI({
    selectInput(ns("select_column"),
                "Select data to plot",
                colnames(data_frame),
                selected = "MS/MS Identified [%]" 
    )
  })
  
  
  if(!is.null(meta)){
    # generate ui if meta data provided
    output$ui_select_grouping <- renderUI({
      selectizeInput(ns("select_group"),
                     "Select grouping",
                     c(colnames(meta)[-1]),
                     multiple = TRUE, options = list(maxItems = 1)
      )
    })
  }else{
    # generate ui for highlight rows/points
    output$ui_select_grouping <- renderUI({
      selectInput(ns("select_row"),
                  "Select data to highlight",
                  rownames(data_frame),
                  multiple=TRUE,
                  size = 10,
                  selectize=FALSE
      )
    })
    
  }
  
  observe({
    if(!is.null(input$select_column)){
      data_frame_for_plot <- select(data_frame, 1,input$select_column)
      rownames(data_frame_for_plot) <- data_frame_for_plot[,1]
    }
    
    
    
    # if meta table provided
    if(!is.null(meta) && !is.null(input$select_group)){
      
      meta_selected <- dplyr::select(meta, 1,input$select_group)
      
      rownames(meta_selected) <- meta_selected[,1]
      # only if meta has all the rawfiles
      if(all(data_frame_for_plot[,1] %in% meta_selected[,1])){
        #
        data_frame_for_plot <- merge(data_frame_for_plot,meta_selected, by.x = 1, by.y =1 )
        row.names(data_frame_for_plot) <- data_frame_for_plot[,1]
        
      }else{
        print("meta information not matching")
      }
      # print(head(data_frame_for_plot))
    }
    
    
    # if highlight rows selected
    if(!is.null(input$select_row)){
      
      if_selected <- as.character(rownames(data_frame) %in% input$select_row)
      if_selected <- dplyr::recode(if_selected, "TRUE" = "selected", "FALSE" = "Not Selected")
      data_frame_for_plot$highlight <- if_selected
      #print(head(data_frame_for_plot))
    }
    #print(dim(data_frame_for_plot))
    
    
    if(input$button_plot > 0){
      # put everything in isolate
      isolate({
        # if meta data provided, and  effective grouping information selected
        if(!is.null(input$select_group)){
          mq_summary <-  MQ_QC_plot(data_frame_for_plot, 
                                    plot_type = input$plot_type, 
                                    group = input$select_group,
                                    cutoff = as.numeric(input$Threshold_line_value),
                                    maintitle = input$select_column,
                                    vertical =  input$if_vertical,
                                    xlabel = input$select_column)[[1]]
          # if no meta data provided, but user select rows to highlight 
        }else if(!is.null(input$select_row)){
          mq_summary <-  MQ_QC_plot(data_frame_for_plot, 
                                    plot_type = input$plot_type, 
                                    group = "highlight",
                                    cutoff = as.numeric(input$Threshold_line_value),
                                    maintitle = input$select_column,
                                    vertical =  input$if_vertical,
                                    xlabel = input$select_column)[[1]]
          # if no meta and no highlight 
        }else if(!is.null(input$select_column)){
          #print(dim(data_frame_for_plot))
          mq_summary <-  MQ_QC_plot(data_frame_for_plot, 
                                    plot_type = input$plot_type, 
                                    cutoff = as.numeric(input$Threshold_line_value),
                                    maintitle = input$select_column,
                                    vertical =  input$if_vertical,
                                    xlabel = input$select_column)[[1]]
        }
        
        # call the module to visualize in the browser
        callModule(plotInBox, "mq_summary",
                   plot_object = mq_summary,
                   name_tag = "mq_summary",
                   plot_type = "ggplot2")
        
        output$ui_plot_output <- renderUI({
          plotInBox_UI(ns("mq_summary"), boxwidth = 8)
        })
        
      })
    }
  })
}



#  scatter3d plot 
# example

# library(shiny)
# library(shinydashboard)
# source("shiny_modules_in_development.R")
# 
# ui <- dashboardPage(
#   dashboardHeader(title = "3d plot"),
#   dashboardSidebar(
#     sidebarMenu(id="tabs",
#                 menuItem("Menu item1", tabName="m1", icon = icon("calendar"))
#     )
#   ),
#   dashboardBody(
#     tabItems(
#       tabItem(tabName = "m1", 
#               
#               fluidRow( # this fluidrow is to ensure the extend the height
#                 SCATTER_3D_UI("demo")
#               )
#       )
#     )
#   )
# )
# server <- function(input, output,session) {
#   my_data <- iris
#   observe({
#     callModule(SCATTER_3D, "demo", data_frame = my_data)
#   })
# }
# shinyApp(ui, server)
# 
# 
# 



SCATTER_3D_UI <-function(id,layout = "2column"){ # layout, 2column for two column, and 1column for one column calling mode
  ns <- NS(id)
  
  switch(layout,
         "1column" = {setting_panel_width <- 12
         },
         "2column" = {setting_panel_width <- 5
         }
  )
  
  fluidRow(
   column(
    setting_panel_width,
    box(
      width = 12,
      solidHeader = TRUE,
      status = "primary",
      # use wellPanel to organize related option together
      wellPanel(
        fluidRow(
          column(6,
                 uiOutput(ns("ui_select_data")),
                 checkboxInput(ns("add_plane_3d"),
                               "Add plane based on linear model 'z ~ x+y' ",
                               FALSE
                 )
          ),
          
          column(6,
                 sliderInput(ns("angle"),
                             "Rotation (angle between x and y axis)",
                             min =0, max = 90, value = 55, step = 1
                 ),
                 sliderInput(ns("scale_y"),
                             "Thickness (length of y axis)",
                             min =0.1, max = 2, value = 0.5, step = 0.1
                 )
          )
        )
      ),

      wellPanel(
        fluidRow(
          column(6,
                 checkboxInput(ns("map_grouping_data"),
                               strong("Map group data on plot?"),
                               FALSE
                               )
          ),
          column(6,
                 conditionalPanel(
                   condition = paste0("input['", ns("map_grouping_data"), "']"),
                   uiOutput(ns("ui_select_group_mapping"))
                 )
                 
          )
        ),
        
        fluidRow(
          column(6,
                 uiOutput(ns("ui_select_pch")),
                 conditionalPanel(
                   condition = paste0("input['", ns("map_grouping_data"), "']"),
                   uiOutput(ns("ui_select_color_theme"))
                 ),
                 uiOutput(ns("ui_select_color")),
                 uiOutput(ns("ui_select_linear_mapping"))
          ),
          column(6,
                 conditionalPanel(
                   condition = paste0("input['", ns("map_grouping_data"), "']"),
                   uiOutput(ns("ui_select_legend_position"))
                 )
                 
                 
          )
          
        )
      ),
      
      fluidRow(
        column(6,
          checkboxInput(ns("more_options"),
                        "Show more general 3d plot options",
                        FALSE
                        )     
        )
        
      ),
      conditionalPanel(
        condition = paste0("input['", ns("more_options"), "']"),
        wellPanel(
          fluidRow(
            column(6,
                   textInput(ns("main"),
                             "main title",
                             "3D Scatter Plot"
                   ),
                   textInput(ns("xlab"),
                             "x label",
                             ""
                   ),
                   sliderInput(ns("symbol_size"),
                               "Symbol size",
                               min = 0.1,
                               max = 4,
                               value = 2,
                               step = 0.1
                               ),
                   checkboxInput(ns("show_axis"),
                                 "show/hide axis",
                                 TRUE
                                 ),
                   # checkboxInput(ns("show_grid"),
                   #               "show/hide grid",
                   #               TRUE
                   # ),
                   selectizeInput(ns("show_grid"),
                                  "show grid on selected facet",
                                  c("xy", "yz", "xz"),
                                  selected = c("xy", "yz", "xz"),
                                  multiple =TRUE,
                                  options=list(plugins=list('drag_drop','remove_button'))
                                  
                   )
                   
                   
            ),
            column(6,
                   textInput(ns("ylab"),
                             "y label",
                             ""
                   ),
                   textInput(ns("zlab"),
                             "z label",
                             ""
                   ),
                   
                   selectizeInput(ns("plot_type"),
                                  "Plot type",
                                  c("separate points" = "p",
                                    "Vertical lines" = "h")
                   ),
                   checkboxInput(ns("show_box"),
                                 "show/hide 3d box",
                                 FALSE
                   ),
                   checkboxInput(ns("highlight"),
                                 "color code according to y coordinates (conflict to grouping)",
                                 FALSE
                   )
                   
                   
            )
          )
        )
      ),
      
      fluidRow(
        column(6),
        column(6,
               actionButton(ns("button_plot"), 
                            icon = icon("paper-plane"),
                            label = "Plot",
                            style="float:right; color: #fff; background-color: #337ab7; border-color: #2e6da4"
               )
        )
      )  
    ),
    
    # here is the plot output, which is already in box
    if(layout == "1column"){ # for one colmn layout
      uiOutput(ns("ui_plot_output"))
    }
   ),
   # here is the plot output, which is already in box
   if(layout == "2column"){ # for two columns layout
     column(7,
       uiOutput(ns("ui_plot_output"))
     )
   }
  )
}



SCATTER_3D <- function(input, output, session, data_frame){
  ns <- session$ns
  
  library(scatterplot3d)
  # add new column names if there is no 
  if(is.null(colnames(data_frame))){
    colnames(data_frame) <- paste0("col_", 1:ncol(data_frame))
  }
  
  # generate ui for data selection
  output$ui_select_data <- renderUI({
    selectizeInput(ns("select_xyz"),
                   "Select xyz (Only choose 3 for 3D plot)",
                   colnames(data_frame),
                   selected = colnames(data_frame)[1:3],
                   multiple =TRUE,
                   options=list(plugins=list('drag_drop','remove_button'))
                   )
  })
  # generate ui for linear mapping for color transparency
  output$ui_select_linear_mapping <- renderUI({
    selectizeInput(ns("select_linear_mapping"),
                   "Select data to scale color tranparency",
                   colnames(data_frame),
                   #selected = NULL,
                   multiple =TRUE,
                   options=list(maxItems = 1, plugins=list('remove_button'))
                   
    )
    
  })
  # generate ui for group data mapping
  output$ui_select_group_mapping <- renderUI({
    selectizeInput(ns("select_group_mapping"),
                   "Select data of grouping",
                   colnames(data_frame),
                   #selected = NULL,
                   multiple =TRUE,
                   options=list(maxItems = 1)
                   
    )
    
  })
  
  # render legend options output
  output$ui_select_legend_position <- renderUI({
    tagList(
      checkboxInput(ns("show_legend"),
                    "Show/hide legend?",
                    TRUE
      ),
      selectInput(ns("Legend_position"),
                  "Legend position",
                  c("bottomright", "bottom", "bottomleft", 
                    "left", "topleft", "top", 
                    "topright", "right", "center"),
                  selected = "topright"
                  
      ),
      # sliderInput(ns("legend_pt_cex"),
      #            "Symbol size",
      #            min = 0.5,
      #            max = 3,
      #            step = 0.1,
      #            value = 2
      #            ),
      sliderInput(ns("legend_text_cex"),
                  "Text size",
                  min = 0.5,
                  max = 3,
                  step = 0.1,
                  value = 1
      ),
      sliderInput(ns("legend_inset"),
                  "Distance from border",
                  min = 0,
                  max = 0.5,
                  step = 0.01,
                  value = 0
      )
    )
  })
  
  
  # preset the piont symbol list
  pch_list <- c(
    "solid square" = 15, 
    "solid circle" = 16, 
    "solid triangle point-up" = 17, 
    "solid diamond" = 18,
    "square" = 0,
    "circle" = 1,
    "triangle point up" = 2,
    "plus" = 3,
    "cross" = 4,
    "diamond" = 5,
    "triangle point down" = 6,
    "square cross" = 7,
    "star" = 8,
    "diamond plus" = 9,
    "circle plus" = 10,
    "triangles up and down" = 11,
    "square plus" = 12,
    "circle cross" = 13,
    "square and triangle down" = 14

    #, 
    ## these symbols are filled with color, do not bother
    #"solid circle" = 19, 
    # "bullet (smaller circle)" = 20,
    # "filled circle" = 21, 
    # "filled square" = 22, 
    # "filled diamond" = 23, 
    # "filled triangle point-up" = 24, 
    # "filled triangle point down" = 25
)

  #  show the color selection theme panel
  # get the group dependent point symbol and color 
  observe({
    # if no grouping is defined, pch and color are going to be the same to all points
    # if the checkbox for grouping is checked and grouping data selected,
    if(input$map_grouping_data && !is.null(input$select_group_mapping) ){
      # if there is grouping column defined
      groups <-  unique(data_frame[,input$select_group_mapping])
      
      output$ui_select_pch <-renderUI({
        selectizeInput(ns("pch"),
                       "Choose Point symbol",
                       pch_list,
                       selected = 0:(length(groups)-1),
                       multiple =TRUE,
                       options=list(plugins=list('drag_drop','remove_button'))
        )
      })
      
      output$ui_select_color_theme <- renderUI({

        selectInput(ns('color_theme'), 
                    'Choose color themes',
                    c('Set1'='Set1',
                      'Set2'='Set2',
                      'Set3'='Set3',
                      'Accent'='Accent',
                      'Pastel1'='Pastel1',
                      'Pastel2'='Pastel2',
                      'Paired' = 'Paired'),
                    'Set2'# preset
                    )
        
      })
      # generate the ui for each group factor
      output$ui_select_color <- renderUI({

        if(!is.null(input$color_theme)){
          preset_colors <- brewer.pal(length(groups),input$color_theme)
          # debug
          print(preset_colors)
          
          colorinput_ui_color_list <- future_lapply(1:length(groups), function(i) {
            tags$div(
              colourInput(ns(paste("col", i, sep="_")), 
                          paste("Set colour for", groups[i], sep=" "), 
                          preset_colors[i],
                          showColour = "background")
            )
          })
          # Convert the list to a tagList - this is necessary for the list of items
          # to display properly.
          do.call(tagList, colorinput_ui_color_list)   
          
        }
      }) 
    }else{
      output$ui_select_pch <-renderUI({
        selectizeInput(ns("pch"),
                       "point symbol",
                       pch_list,
                       selected = 16,
                       #multiple =TRUE,
                       options=list(plugins=list('drag_drop','remove_button'))
        )
      })
      
      output$ui_select_color <-renderUI({
        colourInput(
          ns("single_color"), 
          "Choose color", 
          "#FF0000" # default color as red
        )
      })
      
    }
    
  })
  

  # start plotting
  observe({
    
    if(input$button_plot > 0){
      # put everything in isolate
      # get some addtional paraameters
      isolate({
        if(input$xlab == ""){
          xlab <- colnames(data_frame)[1]
        }else{
          xlab <- input$xlab
        }
        
        if(input$ylab == ""){
          ylab <- colnames(data_frame)[2]
        }else{
          ylab <- input$ylab
        }
        
        if(input$zlab == ""){
          zlab <- colnames(data_frame)[3]
        }else{
          zlab <- input$zlab
        }
        
        # get color and shap mapping 
        if(input$map_grouping_data && !is.null(input$select_group_mapping)){
          # if grouping information is defined
          number_of_groups <- length(unique(data_frame[,input$select_group_mapping]))
          #print(input$pch)
          if(length(input$pch) == number_of_groups){
            shapes <-  as.numeric(input$pch)[as.numeric(data_frame[[input$select_group_mapping]])]
          }else if(length(input$pch) > number_of_groups){
            shapes <-  as.numeric(input$pch[1:number_of_groups])[as.numeric(data_frame[[input$select_group_mapping]])]
          }else if(length(input$pch) ==1){
            shapes <- rep(as.numeric(input$pch), nrow(data_frame))
          }
          
          # get the colors from the list
          mycolor_discret <- unlist(future_lapply(1:number_of_groups, function(i){
                colid <- paste("col", i, sep = "_")
                input[[colid]]}
              ))
          # generate the full length of colors
          colors <-  mycolor_discret[as.numeric(data_frame[[input$select_group_mapping]])]
          #print(colors)

        }else{
          # if no grouping is defined, pch and color are going to be unified
          shapes <- rep(as.numeric(input$pch), nrow(data_frame))
          colors <- rep(input$single_color, nrow(data_frame))  
        }
        
        # scale color transparency if the scaling factor is defined
        if(!is.null(input$select_linear_mapping)){
          color_tranparent_scale_factor <- range_standarize_1(as.numeric(data_frame[[input$select_linear_mapping]]))
          colors <-  unlist(future_lapply(1:length(colors),function(x) add.alpha(colors[x], color_tranparent_scale_factor[x])))
        }
       
        # plotting final
        svg(tempfile())
        dev.control('enable')
        
        # plot without points
        try(s3d <- scatterplot3d(data_frame[,input$select_xyz],
                      angle =  as.numeric(input$angle),
                      pch =  "",
                      #color = colors,
                      main = input$main,
                      xlab =  xlab,
                      ylab =  ylab,
                      zlab =  zlab,
                      #cex.symbols = input$symbol_size,
                      axis =  input$show_axis,
                      grid =  FALSE,
                      #grid =  input$show_grid,
                      type =  input$plot_type,
                      highlight.3d =input$highlight,
                      box =  input$show_box,
                      scale.y = input$scale_y
                      )
        )
        
        # Add grids
        if(!is.null(input$show_grid)){
          addgrids3d(data_frame[,input$select_xyz], 
                     grid = input$show_grid,
                     scale.y = input$scale_y, 
                     angle = as.numeric(input$angle)
                     )
        }
        
        # Add points
        s3d$points3d(data_frame[,input$select_xyz], 
                     pch = shapes,
                     cex = input$symbol_size, # note that the cex and col are diffent names from the scatter plot 3d
                     col = colors
                     )
        
        # plot the plane        
        if(input$add_plane_3d){
          my.lm <- lm(data_frame[,input$select_xyz[3]] ~ data_frame[,input$select_xyz[1]] + data_frame[,input$select_xyz[2]])
          s3d$plane3d(my.lm, draw_polygon = TRUE)
        }
        
        # add legend
        # use two layers of "if" to deal with this situiation
        # otherise, if directly check input$show_legend, will be error if there is no grouping
        if(input$map_grouping_data && !is.null(input$select_group_mapping) && input$show_legend){
            try(legend(input$Legend_position, 
                   legend = levels(data_frame[,input$select_group_mapping]),
                   col = mycolor_discret, 
                   pch = as.numeric(input$pch), 
                   pt.cex = input$symbol_size,
                   cex = input$legend_text_cex,
                   #bty = "n",
                   inset = input$legend_inset))
        }
        
        
        
        
        scatter_3d <- recordPlot()
        dev.off()
        
        callModule(plotInBox, "scatter_3d",
                   plot_object = scatter_3d,
                   name_tag = "scatter_3d",
                   plot_type = "recordPlot")
        
        output$ui_plot_output <- renderUI({
          plotInBox_UI(ns("scatter_3d"), boxwidth = 12)
        })
        
      })
    }
    
  })
}





# upload data module 

# minimum sample with error message catch


# library(shiny)
# library(shinydashboard)
# source("shiny_modules_in_development.R")
# 
# 
# ui <- dashboardPage(
#   dashboardHeader(title = "Dynamic sidebar"),
#   dashboardSidebar(
#     sidebarMenu(id="tabs",
#                 menuItem("Menu item1", tabName="m1", icon = icon("calendar")),
#                 menuItem("Menu item2", tabName="m2", icon = icon("calendar"))
#     )
#   ),
#   dashboardBody(
#     tabItems(
#       tabItem(tabName = "m1", 
#               fluidRow( # this fluidrow is to ensure the extend the height
#                 tabBox(
#                   id ="demo",
#                   width = 12,
#                   tabPanel("Upload test",
#                            fluidRow(
#                              UPLOAD_TABLE_UI("demo")
#                            )
#                   )
#                 )
#               )
#       ),
#       tabItem(tabName = "m2",
#               fluidRow(
#                 column(6,
#                        pre(id = "console_error") # this is the place to dispay the error messagye
#                 )
#               )
#       )
#       
#     )
#   )
# )
# server <- function(input, output,session) {
#   
#   observe({
#     withConsoleRedirect("console_error",{
#       callModule(UPLOAD_TABLE, "demo") # call this within the withConsoleRedirect will catch the error message and display
#     })
#     
#   })
# }
# shinyApp(ui, server)




UPLOAD_TABLE_UI <-function(id, 
                           header =  TRUE, 
                           rownames =  TRUE, 
                           boxwidth = 12,
                           boxtitle = "Upload data file (tsv/csv)" ){
  ns <- NS(id)
  
  box(title = boxtitle,
      status = "primary", 
      width =  boxwidth,
      solidHeader = TRUE,
      collapsible = TRUE,
      fluidRow(
        column(6,
               checkboxInput(ns('header'), 'First line is Header?', header),
               selectInput(ns('quote'), 'Quote type',
                           c(None='',
                             'Double Quote'='"',
                             'Single Quote'="'"),
                           '')
        ),
        column(6, 
               checkboxInput(ns('rowname'), 'First column is rownames?', rownames),
               selectInput(ns('sep'), 'Deliminator',
                           c(Comma=',',
                             Semicolon=';',
                             Tab='\t'),
                           '\t')
        )
      ),
      fileInput(ns('file'), 'Browse/drag-and-drop your data file here: ',
                accept = c(
                  'text/csv',
                  'text/comma-separated-values',
                  'text/tab-separated-values',
                  'text/plain',
                  '.csv',
                  '.tsv'
                )
      )
      ,
      conditionalPanel(
        condition = paste0("output['", ns("data_upload_status"), "']"),
        DATATABLE_Display_SIMPLE_UI(ns("upload_data_display")) #using the simple databale display module in the same ui
      )
      
      
  )
}

UPLOAD_TABLE <- function(input, output, session, display_after = TRUE){
  ns <- session$ns
  inFile <- input$file
  data_upload <- reactive({
    
    if(is.null(inFile$datapath)){ # only if there the path is a string
      return(NULL) # this is alway the first line when reading in a file
    }else{
      #print(inFile)
      if(input$rowname){
        try(read.delim(inFile$datapath, header=input$header, sep=input$sep, 
                       quote=input$quote, row.names = 1))
      }else{
        try(read.delim(inFile$datapath, header=input$header, sep=input$sep, 
                       quote=input$quote))
      }
    }  
  })
  
  output$data_upload_status <- reactive({
    return(!is.null(data_upload()) && display_after)
  })
  outputOptions(output, 'data_upload_status', suspendWhenHidden = FALSE)
  # do not use ns() here, 
  # do not use suspendWhenHidden = TRUE, because this status is always hidden
  
  if(!is.null(data_upload())){
    
    callModule(DATATABLE_Display_SIMPLE, "upload_data_display",
               data_table = data_upload(),
               height = 200)
  }
  
  
  data_upload() # return the expression, not the value
  
  
}

# ui2 is a slight modification of the version 1, with addition of check.names option, while removing the quote options

UPLOAD_TABLE_UI2 <-function(id, 
                            header =  TRUE, 
                            rownames =  TRUE, 
                            boxwidth = 12,
                            check.names =  TRUE,
                            collapsed =  FALSE,
                            collapsible = TRUE,
                            solidHeader = TRUE,
                            status = "primary", 
                            boxtitle = "Upload data file (tsv/csv)" ){
  ns <- NS(id)
  
  box(title = boxtitle,
      status = status, 
      width =  boxwidth,
      solidHeader = solidHeader,
      collapsible = collapsible,
      collapsed = collapsed,
      fluidRow(
        column(6,
               checkboxInput(ns('header'), 'First line as Header?', header),
               checkboxInput(ns('check_names'), 'Follow R naming rules(to remove special symbols)?', check.names)

        ),
        column(6, 
               checkboxInput(ns('rowname'), 'First column as rownames?', rownames),
               selectInput(ns('sep'), 'Deliminator',
                           c(Comma=',',
                             Semicolon=';',
                             Tab='\t'),
                           '\t')
        )
      ),
      
      fileInput(ns('file'), 'Browse/drag-and-drop your data file here:',
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
        condition = paste0("output['", ns("data_upload_status"), "']"),
        DATATABLE_Display_SIMPLE_UI(ns("upload_data_display")) #using the simple databale display module in the same ui
      ),
      br(),
      verbatimTextOutput(ns("console"), placeholder = FALSE)
      
      
  )
}


# this module has the console output 
# 
UPLOAD_TABLE2 <- function(input, output, session, display_after = TRUE){
  ns <- session$ns
  inFile <- input$file
  data_upload <- reactive({
    
    if(is.null(inFile$datapath)){ # only if there the path is a string
      return(NULL) # this is alway the first line when reading in a file
    }else{
      #print(inFile)
      if(input$rowname){
        try(
          withCallingHandlers(
            read.delim(inFile$datapath, header=input$header, sep=input$sep,
                      check.names =  input$check_names, row.names = 1),
            # can use "warning" instead/on top of "message" to catch warnings too
            {warning("This is a warning message")
              message("This is a message")
              },
            message = function(m) {
              shinyjs::html("console", m$message, TRUE)
            },
            warning = function(m) {
              shinyjs::html("console", m$message, add = TRUE)
            }
            
          )
        )
      }else{
        try(
          withCallingHandlers(
            try(read.delim(inFile$datapath, header=input$header, sep=input$sep, 
                           check.names =  input$check_names)),
            # can use "warning" instead/on top of "message" to catch warnings too
            message = function(m) {
              shinyjs::html("console", m$message, TRUE)
            },
            warning = function(m) {
              shinyjs::html("console", m$message, add = TRUE)
            }
            
          )
        )
      }
    }  
  })
  
  output$data_upload_status <- reactive({
    return(!is.null(data_upload()) && display_after)
  })
  outputOptions(output, 'data_upload_status', suspendWhenHidden = FALSE)
  # do not use ns() here, 
  # do not use suspendWhenHidden = TRUE, because this status is always hidden
  
  if(!is.null(data_upload())){
    
    callModule(DATATABLE_Display_SIMPLE, "upload_data_display",
               data_table = data_upload(),
               height = 200)
  }
  
  
  data_upload() # return the expression, not the value
  
  
}



## paste list module , not working yet-
# 
# PASTELIST_UI <-function(id){
#   ns <- NS(id)
#   fluidRow(
#     box(solidHeader = TRUE,
#         title = "Paste list", 
#         status = "primary", 
#         width = 12,
#         "Sample files for download (right click to save as): ", 
#         br(),
#         a(href = 'proteinlist_sample1.txt',target = '_blank', 'proteinlist_sample1.txt'),"; ",
#         a(href = 'proteinlist_sample2.txt',target = '_blank', 'proteinlist_sample2.txt'),"; ",
#         a(href = 'proteinlist_sample3.txt',target = '_blank', 'proteinlist_sample3.txt')
#     ),
#     box(
#       width = 12,
#       status = "primary",
#       solidHeader = TRUE,
#       actionButton(ns("load_sampmle_data_Button"), 
#                    icon = icon("download"),
#                    label = "Load sample list",
#                    style="color: #fff; background-color: #337ab7; border-color: #2e6da4"
#       ),
#       actionButton(ns("removelist"), 
#                    label = "Remove last list",
#                    style="float:right"
#       ),
#       actionButton(ns("addlist"), 
#                    label = "Add one more list",
#                    style="float:right"
#       )
#     ),
#     
#     box(
#       width = 12,
#       status = "primary",
#       solidHeader = TRUE,
#       column(3,
#              textInput(ns("name_list1"), NULL, "inputlist1"),
#              textAreaInput(ns("inputlist1"), NULL, height = "300px",width = "100%")
#       ),
#       column(3,
#              textInput(ns("name_list2"), NULL, "inputlist2"),
#              textAreaInput(ns("inputlist2"), NULL, height = "300px",width = "100%")
#       ),
#       
#       # new element is going to be inserted here 
#       tags$div(id = ns('placeholder')) 
#       
#     ),
#     conditionalPanel(
#       #condition = 'output.textAreainput_condition',
#       condition = paste0("output['", ns("textAreainput_condition"), "']"),
#       
#       fluidRow(
#         box(solidHeader = TRUE,
#             width = 12, 
#             actionButton(ns("noaction"), 
#                          icon = icon("thumbs-up"),
#                          label = "List-pasted Uploaed!"),
#             actionButton(ns("gotoanalysis"), 
#                          icon = icon("arrow-right"),
#                          icon.library = c("font awesome"),
#                          label = "Go to Enrichment Analysis settings page!",
#                          style="float:right; color: #fff; background-color: #337ab7; border-color: #2e6da4")
#             
#         )
# 
#       )                           
#     )
#     
#   )
# }
# PASTELIST <- function(input, output, session){
#   ns <- session$ns
# 
#   # __ dynamic textArea ui 
#   
#   ## keep track of current element ids (existing, inserted and not yet removed)
#   index <- 2 # set the inital id index of the textAreaInput and column element
#   
#   inserted <- c("column1","column2") # record the column ids
#   textAreaInput_ids <- c("inputlist1","inputlist2") # record th textAreaInput ids
#   
#   textinput_ids <- c("name_list1","name_list2") # record th textInput ids
#   
#   list_names <<- reactive({
#     unlist(future_lapply(textinput_ids, function(x){input[[x]]}))
#   }) 
#   
#   # for the inital two list: inputlist1, inputlist2
#   
#   list_textArea_input <<- reactive({
#     t <-future_lapply(textAreaInput_ids, function(x){unlist(strsplit(input[[x]], "\n"))})
#       # split by \n into a character list
#     names(t) <-  list_names()
#     t
#   })
# 
#   # this is to generate a condition to decide if there is any input from the list
#   output$textAreainput_condition <- reactive({
#     !any(unlist(future_lapply(list_textArea_input(), function(x){length(x) == 0})))
#   })
#   
#   
#   #  __ load sample data
#   
#   # load the sample data of 3 protein list
#   observe({
#     if(input$load_sampmle_data_Button >0 && index == 2 ){
#       # read in the list
#       test_list1 <-read.delim("./www/proteinlist_sample1.txt", header =  FALSE)[,1]
#       test_list2 <-read.delim("./www/proteinlist_sample2.txt", header =  FALSE)[,1]
#       test_list3 <-read.delim("./www/proteinlist_sample3.txt", header =  FALSE)[,1]
#       
#       # update the current textArea input
#       index <<- 3 # whennever the button clicked, the index +1 
#       
#       colid <- 'column3' # generate a new column id, 
#       inputId <- 'inputlist3' # generate a new textAreaInput id
#       
#       inserted <<- c("column1", "column2", "column3") # record all column ids
#       textAreaInput_ids <<- c("inputlist1", "inputlist2","inputlist3") # record all textAreaInput ids
#       textinput_ids <- c("name_list1","name_list2","name_list3") # record th textInput ids
#   
#       updateTextAreaInput(session, "inputlist1",
#                           value = paste(test_list1,collapse="\n"))
#       updateTextAreaInput(session, "inputlist2",
#                           value = paste(test_list2,collapse="\n"))
#       
#       
#       # insert a new element
#       insertUI(
#         selector = "#placeholder",
#         where = "beforeEnd",
#         
#         ui = tags$div(
#           column(3,
#                  textInput(ns("name_list3"), NULL, "inputlist3"),
#                  textAreaInput(ns(inputId), NULL, value = paste(test_list3,collapse="\n"), height = "300px",width = "100%")
#           ), 
#           id = ns(colid)
#         )
#       )
#       
#       
#       # here in a block, if not being a return value, and you want to change/update the value
#       # you have to use the <<- to force assign the value, otherwise it will not be updated 
#       # however, if it is for output$XXX, it will be fine, output list is special in shiny
#       
#       list_names <<- reactive({
#         unlist(future_lapply(textinput_ids, function(x){input[[x]]}))
#       })      
#       
#       list_textArea_input <<- reactive({
#         t <-future_lapply(textAreaInput_ids, function(x){
#           unlist(strsplit(input[[x]], "\n"))
#         }
#         )
#         names(t) <-  list_names()
#         t
#       })
#       
#       output$value <- renderPrint({list_textArea_input()})
#       
#       # this is to generate a condition to decide if there is any input from the list
#       output$textAreainput_condition <- reactive({
#         !any(unlist(future_lapply(list_textArea_input(), function(x){length(x) == 0})))
#       })
#     }
#     
#     # reload the list if there are more than 3 list already,
#     if(input$load_sampmle_data_Button > 0 && index >= 3){
#       
#       updateTextAreaInput(session, "inputlist1",
#                           value = paste(test_list1,collapse="\n"))
#       updateTextAreaInput(session, "inputlist2",
#                           value = paste(test_list2,collapse="\n"))
#       updateTextAreaInput(session, "inputlist3",
#                           value = paste(test_list3,collapse="\n"))
#       
#       # since there is no input$xxx, the output$value has to be recalulated
#       list_textArea_input <<- reactive({
#         t <-future_lapply(textAreaInput_ids, function(x){
#           unlist(strsplit(input[[x]], "\n"))
#         }
#         )
#         names(t) <-  list_names()
#         t
#       })
#       
#       
#       # this is to generate a condition to decide if there is any input from the list
#       output$textAreainput_condition <- reactive({
#         !any(unlist(future_lapply(list_textArea_input(), function(x){length(x) == 0})))
#       })
#       
#     }
#   })
#   
#   
#   # add ui:  more protein/function list
#   observeEvent(input$addlist, {
#     index <<- index+1 # whennever the button clicked, the index +1 
#     
#     colid <-paste0('column', index) # generate a new column id, 
#     inserted <<- c(inserted, colid) # record all column ids
#     
#     inputId <- paste0('inputlist', index) # generate a new textAreaInput id
#     textAreaInput_ids <<- c(textAreaInput_ids, inputId) # record all textAreaInput ids
#     
#     textinputId <- paste0('name_list', index) # generate a new textAreaInput id
#     textinput_ids <<- c(textinput_ids, textinputId) # record th textInput ids
#     
#     list_names <<- reactive({
#       unlist(future_lapply(textinput_ids, function(x){input[[x]]}))
#     }) 
#     
#     
#     # insert a new element
#     insertUI(
#       selector = "#placeholder",
#       where = "beforeEnd",
#       
#       ui = tags$div(
#         column(3,
#                textInput(ns(textinputId), NULL, inputId),
#                textAreaInput(ns(inputId), NULL, height = "300px",width = "100%")
#         ), 
#         id = ns(colid)
#       )
#     )
#     
#     # since there is no input$xxx, the output$value has to be recalulated
#     list_textArea_input <<- reactive({
#       t <-future_lapply(textAreaInput_ids, function(x){
#         unlist(strsplit(input[[x]], "\n"))
#       }
#       )
#       names(t) <-  list_names()
#       t
#     })
#     
#     
#     # this is to generate a condition to decide if there is any input from the list
#     output$textAreainput_condition <- reactive({
#       !any(unlist(future_lapply(list_textArea_input(), function(x){length(x) == 0})))
#     })
#     
#     
#   })
#   
#   # remove ui
#   observeEvent(input$removelist, {
#     if(index > 2){
#       # remove the ui first, then operate on the list, otherwise, would remove the second last ui
#       removeUI(
#         ## pass in appropriate div id
#         selector = paste0('#', inserted[length(inserted)]) # remove the last column element
#       )
#       
#       index <<- index-1 # when the button clicked, decrease the index by -1
#       # remove the last element
#       inserted <<- inserted[-length(inserted)] # in the column elements
#       textAreaInput_ids <<- textAreaInput_ids[-length(textAreaInput_ids)] # in the textAreaInput elements
#       textInput_ids <<- textInput_ids[-length(textInput_ids)] # in the textAreaInput elements
#       
#       # re-render the ouput
#       list_textArea_input <<- reactive({
#         t <-future_lapply(textAreaInput_ids, function(x){
#           unlist(strsplit(input[[x]], "\n"))
#         }
#         )
#         names(t) <-  list_names()
#         t
#       })
#       output$value <- renderPrint({list_textArea_input()})
#       # this is to generate a condition to decide if there is any input from the list
#       output$textAreainput_condition <- reactive({
#         !any(unlist(future_lapply(list_textArea_input(), function(x){length(x) == 0})))
#       })
#       
#       
#     }else{
#       index <<- 2 # index cannot be smaller than 1
#     }
#     
#   })
#   
#   outputOptions(output, 'textAreainput_condition', suspendWhenHidden=FALSE)
#   
#   
#   
#   
#   
#   
#   
# }

# For phylo plot 


PHYLOPLOT_UI <-function(id){
  ns <- NS(id)
  fluidRow(
    tabBox(
      #id ="tabBox_plottings", # this is optional
      width = 12,
      
      #  _____ tab: collapsale phylotree
      tabPanel("Collapsible phylotree map", 
               fluidRow(
                 column(4,
                        box(width = 12,
                            solidHeader = TRUE,
                            status = "primary",
                            uiOutput(ns("Hierarchy")),
                            
                            column(6, textInput(ns("Root_name"),
                                                "Root Name",
                                                value = "Root"
                            )),
                            column(6, selectizeInput(ns("layout"),
                                                     "layout",
                                                     c("collapse","radial","cartesian")
                            ))
                            
                        ),
                        box(width = 12,
                            solidHeader = TRUE,
                            status = "primary",
                            tableOutput(ns("clickView"))
                            
                        )
                        #,
                        #DATATABLE_Display_UI(ns("Lineage_selected"), boxtitle = "Lineages below selected")
                        
                        
                 ),
                 column(8,
                        box(
                          width = 12,
                          solidHeader = TRUE,
                          status = "primary",
                          d3treeOutput(outputId=ns("d3"),width = "100%",height = '800px')
                        )
                 )
               )
      ),
      
      
      
      #  _____ tab:  phylotree
      
      tabPanel( 
        "phylotree map", 
        fluidRow(
          box(
            width = 4,
            status = "primary",
            solidHeader = TRUE,
            fluidRow(
              column(6,
                     selectInput(ns("phylo_tree_type"),
                                 "Phylo tree type:",
                                 c("Diagonal", "Radial")
                     ),
                     colourInput(ns("phylo_tree_line_color"), 
                                 "Line color", 
                                 "#ccc"),
                     colourInput(ns("phylo_tree_node_color"), 
                                 "Node filling color", 
                                 "#fff"),
                     colourInput(ns("phylo_tree_node_outlinecolor"), 
                                 "Node outline color", 
                                 "#4A74B3"),
                     colourInput(ns("phylo_tree_text_color"), 
                                 "Text color", 
                                 "#111")
              ),
              column(6,
                     sliderInput(ns("phylo_tree_opacity"),
                                 "Opacity",
                                 min = 0.1,
                                 max = 1,
                                 value = 0.9),
                     numericInput(ns("phylo_tree_fontsize"),
                                  "font Size:",
                                  value = 10
                     )
                     
              )
            ),
            
            fluidRow(
              column(6),
              column(6,
                     actionButton(ns("button_apply_phylotree_map"), 
                                  icon = icon("paper-plane"),
                                  label = "Plot",
                                  #style="float:right"
                                  style="float:right; color: #fff; background-color: #337ab7; border-color: #2e6da4"
                     )
              )
              
            )
          ),
          uiOutput(ns("ui_phylotree_map"))
        )
      ),
      #  _____ tab:  sunburst
      
      tabPanel( 
        "Sunburst", 
        fluidRow(
          box(
            width = 4,
            status = "primary",
            solidHeader = TRUE,
            fluidRow(
              column(6,
                     uiOutput(ns("ui_sunburst_index"))
                     
              ),
              column(6
                     #,
                     
              ))
            ,
            
            fluidRow(
              column(6),
              column(6,
                     actionButton(ns("button_plot_sunburst"), 
                                  icon = icon("paper-plane"),
                                  label = "Plot",
                                  #style="float:right"
                                  style="float:right; color: #fff; background-color: #337ab7; border-color: #2e6da4"
                     )
              )
              
            )
          ),
          
          uiOutput(ns("ui_sunburst"))
          
          
        )
      ),
      #  _____ tab:  interactive hirarchical
      
      tabPanel( 
        "Interactive hirarchical plot", 
        fluidRow(
          box(
            width = 4,
            status = "primary",
            solidHeader = TRUE,
            fluidRow(
              column(6,
                     uiOutput(ns("ui_d3partitionR_index")),
                     radioButtons(ns("d3partitionR_plottype"),
                                  "Choose plot type",
                                  c("Interactive sunbust"='sunburst',
                                    "MetaMap circle tree"='circle_treemap',
                                    #"Classical treemap" = 'treemap',
                                    "Horizontal Partition bar" = 'icicle',
                                    "Vertical Partition bar"= 'partition_chart'),
                                  selected = "circle_treemap"
                     )
                     
              ),
              column(6,
                     textInput(ns("d3partition_rootname"),
                               "Root name",
                               "Life tree"
                     ),
                     sliderInput(ns("d3partition_label_fontsize"),
                                 "Label font size",
                                 min = 0.1,
                                 max = 5,
                                 value = 2)
                     
              ))
            ,
            
            fluidRow(
              column(6),
              column(6,
                     actionButton(ns("button_apply_D3partitionR"), 
                                  icon = icon("paper-plane"),
                                  label = "Plot",
                                  #style="float:right"
                                  style="float:right; color: #fff; background-color: #337ab7; border-color: #2e6da4"
                     )
              )
              
            )
          ),
          
          uiOutput(ns("UI_viz_D3partitionR"))
          
        )
      ),
      #  _____ tab:  rectangle treemap
      
      tabPanel( 
        "Interactive Rectangle TreeMap", 
        fluidRow(
          box(
            width = 4,
            status = "primary",
            solidHeader = TRUE,
            fluidRow(
              column(6,
                     uiOutput(ns("ui_treemap_index")),
                     uiOutput(ns("ui_treemap_vcolor")),
                     textInput(ns("treemap_root_name"),
                               "Root name",
                               "Bacteria"
                     ),
                     selectInput(ns("treemap_color_type"),
                                 "How the rectangles are colored",
                                 c("by branch" = "index", "by level"= "depth")
                     )
                     
                     
              ),
              column(6,
                     selectInput(ns("treemap_algorithm"),
                                 "treemap algorithm:",
                                 c( "Good aspect ratios, but ignores the sorting orde"= "squarified",
                                    "Takes the sorting order into account while aspect ratios are still acceptable" = "pivotSize")
                     ),
                     h5("This plot is conficting with the phylo tree, whichever plot first will show up.")
              )
            ),
            
            fluidRow(
              column(6),
              column(6,
                     actionButton(ns("button_apply_rectangle_treemap"), 
                                  icon = icon("paper-plane"),
                                  label = "Plot",
                                  style="float:right; color: #fff; background-color: #337ab7; border-color: #2e6da4"
                     )
              )
              
            )# fluidRow ends
          ),# left box ends
          uiOutput(ns("ui_treemap_display"))
        )
      )
      #end of tab
    ),
    shinysky::busyIndicator(text = "Loading, please wait ...",
                            img = "ajaxloaderq.gif",
                            wait = 500) 
  )
}

# data could be ajacent "lineage" or "taxonid"
# currently only linage data.frame with columns of hirachical data and last column of value supported
# will suport taxon id list and value column after

PHYLOPLOT <- function(input, output, session, data, type = "lineage"){
  ns <- session$ns
  # install/load required libraries
  install.packages.auto(treemap) # for treemap
  install.packages.auto(d3treeR)
  install.packages.auto(data.tree)
  install.packages.auto(D3partitionR)
  install.packages.auto(sunburstR)
  install.packages.auto(dplyr)
  
  # take over the data
  data_frame_for_plot <- data
  
  #  _____ collapsale phylotree
  
  # pre formating the hirachical data.frame  
  m <- data_frame_for_plot
  
  rownames(m) <- 1: nrow(m)
  m <- m[,-ncol(m)]# remove the intensity column
  m <- remove_1st_column(m) # remove the preceding columns with only 1 unique value
  m[m == ""] <- NA # replace blanks with NA for the ease of na remoal
  m <- remove_allNA_rows(m) # remve any all NA rows if there are any
  m[is.na(m)] <- " "
  
  m$NEWCOL <- NA # add one new column for the filtering
  
  
  
  # render the UI for level selection
  output$Hierarchy <- renderUI({
    Hierarchy=names(m)
    Hierarchy=head(Hierarchy,-1)
    selectizeInput(ns("Hierarchy"),"Tree Hierarchy",
                   choices = Hierarchy,multiple=T,selected = Hierarchy,
                   options=list(plugins=list('drag_drop','remove_button')))
  })
  
  network <- reactiveValues()
  #  using js to update the selection
  observeEvent(input$d3_update,{
    network$nodes <- unlist(input$d3_update$.nodesData)
    activeNode<-input$d3_update$.activeNode
    if(!is.null(activeNode)) network$click <- jsonlite::fromJSON(activeNode)
  })
  
  observeEvent(network$click,{
    output$clickView<-renderTable({
      as.data.frame(network$click)
    },caption='Last Selected Node',caption.placement='top')
  })
  
  
  TreeStruct = eventReactive(network$nodes,{
    df=m
    if(is.null(network$nodes)){
      df=m
    }else{
      
      x.filter = tree.filter(network$nodes,m)
      #print(x.filter)
      
      df = ddply(x.filter,.(ID),function(a.x){
        m%>%filter_(.dots = list(a.x$FILTER))%>%distinct
      }
      )
    }
    df
  })
  
  # render the plot
  
  observeEvent(input$Hierarchy,{
    output$d3 <- renderD3tree({
      if(is.null(input$Hierarchy)){
        p=m
      }else{
        p=m%>%select(one_of(c(input$Hierarchy,"NEWCOL")))%>%unique
      }
      
      d3Tree::d3tree(data = list(root = d3Tree::df2tree(struct = p,
                                                        rootname = input$Root_name),
                                 layout = input$layout),
                     activeReturn = c('name','value','depth')
      )
    })
  })
  
  
  # #_____ for data table display: selected notes' downstream
  # observe({
  #   # data = try(TreeStruct()%>%select(-NEWCOL))
  #   # print(head(data))
  #   
  #   #if(!is.null(data)){
  #     callModule(DATATABLE_Display, "Lineage_selected",
  #                data_table = TreeStruct()[,-ncol(TreeStruct())],
  #                filename_tag = "Lineage_selected",
  #                height = 200)
  #   #}
  # })
  
  
  
  #_____ rectangle treemap
  
  output$ui_treemap_index <- renderUI({
    
    column_names_df <- colnames(data_frame_for_plot)
    selectInput(ns("treemap_index"), 
                "Select the levels to include in the tree map",
                column_names_df[-length(column_names_df)],
                selected = column_names_df[-length(column_names_df)],
                multiple = TRUE
    )
    
  })
  output$ui_treemap_vcolor <- renderUI({
    selectInput(ns("treemap_vcolor"), 
                "Select the level as the principle color",
                colnames(data_frame_for_plot)
    )
    
  })
  
  
  observe({
    if(input$button_apply_rectangle_treemap > 0 ){
      
      isolate({
        tree_df<- as.data.frame(data_frame_for_plot) # just in case not a data.frame
        # plot the tree
        
        #print(colnames(tree_df))
        
        output$tree <- d3treeR::renderD3tree2({
          d3treeR::d3tree2(
            treemap(tree_df,
                    index = input$treemap_index,
                    vSize = "Intensity",
                    vColor = input$treemap_vcolor,
                    type = input$treemap_color_type,
                    title = "my title",
                    algorithm = input$treemap_algorithm,
                    sortID = "size"),
            height = "800px",
            #width = "80%",
            rootname = input$treemap_root_name
          )
        })
        
        # for ui output
        output$ui_treemap_display <- renderUI({
          box(
            width = 8,
            #height = "800px",
            status = "primary",
            solidHeader = TRUE,
            d3tree2Output(ns('tree'), height=800)
            
          )
          
        })
        
      })
    }
    
  })
  
  #_____ phylotree map
  observe({
    if(input$button_apply_phylotree_map >0 ){
      
      tree_df<- as.data.frame(data_frame_for_plot) # just in case not a data.frame
      
      # note that this data.frame' s last column is the intensity
      
      tree_df[tree_df == ""] <- " "
      tree_df[is.na(tree_df)]<- " "
      
      # generate the string for tree converstion
      tree_df$pathString = future_apply(tree_df[1:length(tree_df) -1], 1, function(x) paste(x, collapse = "/"))
      
      my_taxid_tree <- data.tree::as.Node(tree_df, pathDelimiter = "/") # data.tree format
      
      # convert to list first
      my_taxid_tree_list <- data.tree::ToListExplicit(my_taxid_tree, unname = TRUE)
      
      isolate({
        
        # as a classical tree
        #diagonalNetwork(List = my_taxid_tree_list, fontSize = 10, opacity = 0.9)
        
        switch(input$phylo_tree_type,
               "Diagonal" = {
                 output$phylotree_map <- networkD3::renderDiagonalNetwork({
                   # output$Diagonal_tree <- renderPlot({
                   # 
                   networkD3::diagonalNetwork(my_taxid_tree_list,
                                              fontSize = input$phylo_tree_fontsize,
                                              linkColour = input$phylo_tree_line_color,
                                              nodeColour = input$phylo_tree_node_color,
                                              nodeStroke = input$phylo_tree_node_outlinecolor,
                                              textColour = input$phylo_tree_text_color,
                                              opacity  = as.numeric(input$phylo_tree_opacity)
                   )
                   
                 })
                 
                 output$ui_phylotree_map <- renderUI({
                   box(
                     width = 8,
                     #height = "800px",
                     status = "primary",
                     solidHeader = TRUE,
                     diagonalNetworkOutput(ns('phylotree_map'),
                                           height = "1200")
                     
                   )
                 })
                 
               },
               "Radial" = {
                 
                 output$phylotree_map <- networkD3::renderRadialNetwork({
                   
                   networkD3::radialNetwork(my_taxid_tree_list,
                                            fontSize = input$phylo_tree_fontsize,
                                            linkColour = input$phylo_tree_line_color,
                                            nodeColour = input$phylo_tree_node_color,
                                            nodeStroke = input$phylo_tree_node_outlinecolor,
                                            textColour = input$phylo_tree_text_color,
                                            opacity  = as.numeric(input$phylo_tree_opacity)
                                            
                                            
                   )
                   
                 })
                 
                 output$ui_phylotree_map <- renderUI({
                   box(
                     width = 8,
                     status = "primary",
                     solidHeader = TRUE,
                     radialNetworkOutput(ns('phylotree_map'))
                     
                   )
                 })
                 
               }
        )
      })
      
      
      
      
    }
  })
  
  #_____ surnburst
  
  output$ui_sunburst_index <- renderUI({
    
    column_names_df <- colnames(data_frame_for_plot)
    
    colindex <- 1:(length(column_names_df)-1)
    names(colindex) <- column_names_df[-length(column_names_df)]
    
    selectInput(ns("sunburst_index"), 
                "Select the >=2 levels to plot the sunburst",
                colindex,
                selected = colindex,
                multiple = TRUE
    )
    
  })
  
  
  observe({
    if(input$button_plot_sunburst >0 ){
      
      isolate({
        tree_df<- as.data.frame(data_frame_for_plot) # just in case not a data.frame
        #tree_df <-  read.delim("www/phylo_peptide.txt", header = TRUE, row.names = 1)
        tree_df <- tree_df[1:200,]
        # note that this data.frame' s last column is the intensity
        
        # only keep the selected columns to plot
        try(tree_df <- tree_df[,c(as.numeric(input$sunburst_index),ncol(tree_df)), drop = FALSE])
        #tree_df <- tree_df[,c(1:5, ncol(tree_df)), drop = FALSE]
        
        # generate the string of hirachical
        pathString = future_apply(tree_df[1:(ncol(tree_df)-1)], 1, function(x) paste(x, collapse = "-"))
        
        tree_df_for_sunburst <- data.frame(pathString, intensity = tree_df[, ncol(tree_df)])    
        
        output$sunburst <- renderSunburst({
          sunburst(tree_df_for_sunburst,
                   count = TRUE
          )
        })
        
        
        output$ui_sunburst <- renderUI({
          box(
            width = 8,
            #height = "800px",
            status = "primary",
            solidHeader = TRUE,
            sunburstOutput(ns('sunburst'), height = "800px")
            
          )
          
        })
        
      })
    }
    
  })
  
  
  #_____ More hirarchical plot
  
  
  output$ui_d3partitionR_index <- renderUI({
    
    column_names_df <- colnames(data_frame_for_plot)
    
    selectInput(ns("d3partitionR_index"), 
                "Select the >=2 levels to plot the sunburst",
                column_names_df[-length(column_names_df)],
                selected = column_names_df[-length(column_names_df)],
                multiple = TRUE
    )
    
  })
  
  
  observe({
    if(input$button_apply_D3partitionR >0 ){
      
      isolate({
        tree_df<- as.data.table(data_frame_for_plot) # just in case not a data.frame
        
        # remove any na or empty cells
        tree_df[tree_df == ""] <- " "
        tree_df[is.na(tree_df)]<- " "
        names(tree_df)[length(tree_df)] <- "Intensity"
        
        
        #print(input$d3partitionR_index)
        output$viz_D3partitionR<-renderD3partitionR({
          
          D3partitionR() %>%
            add_data(tree_df,
                     count = 'Intensity',
                     steps=input$d3partitionR_index,
                     label='name',
                     tooltip=c('name','Intensity')
                     #color = 'Intensity',
            ) %>%
            add_title(input$d3partition_rootname,
                      style = 'font-size: 200%;'
            ) %>%
            set_chart_type(input$d3partitionR_plottype) %>%
            set_legend_parameters(zoom_subset = TRUE) %>%
            set_labels_parameters(visible = T, 
                                  cut_off = 3,
                                  style =  paste0("font-size: ",as.numeric(input$d3partition_label_fontsize*100),"%")
            ) %>%
            
            set_tooltip_parameters(visible=FALSE, 
                                   style='background-color:lightblue;',
                                   builder='table') %>% 
            plot()
          
        })
        
        output$UI_viz_D3partitionR <- renderUI({
          box(
            width = 8,
            #height = "800px",
            status = "primary",
            solidHeader = TRUE,
            D3partitionROutput(ns('viz_D3partitionR'),height=800)
            
          )
          
        })
        
      })
    }
    
  })
  
  
}


PHYLOPLOT2_UI <-function(id){
  ns <- NS(id)
  fluidRow(
    tabBox(
      #id ="tabBox_plottings", # this is optional
      width = 12,
      
      #  _____ tab:  phylotree
      
      tabPanel( 
        "phylotree map", 
        fluidRow(
          box(
            width = 4,
            status = "primary",
            solidHeader = TRUE,
            fluidRow(
              column(6,
                     selectInput(ns("phylo_tree_type"),
                                 "Phylo tree type:",
                                 c("Diagonal", "Radial")
                     ),
                     colourInput(ns("phylo_tree_line_color"), 
                                 "Line color", 
                                 "#ccc"),
                     colourInput(ns("phylo_tree_node_color"), 
                                 "Node filling color", 
                                 "#fff"),
                     colourInput(ns("phylo_tree_node_outlinecolor"), 
                                 "Node outline color", 
                                 "#4A74B3"),
                     colourInput(ns("phylo_tree_text_color"), 
                                 "Text color", 
                                 "#111")
              ),
              column(6,
                     sliderInput(ns("phylo_tree_opacity"),
                                 "Opacity",
                                 min = 0.1,
                                 max = 1,
                                 value = 0.9),
                     numericInput(ns("phylo_tree_fontsize"),
                                  "font Size:",
                                  value = 10
                     )
                     
              )
            ),
            
            fluidRow(
              column(6),
              column(6,
                     actionButton(ns("button_apply_phylotree_map"), 
                                  icon = icon("paper-plane"),
                                  label = "Plot",
                                  #style="float:right"
                                  style="float:right; color: #fff; background-color: #337ab7; border-color: #2e6da4"
                     )
              )
              
            )
          ),
          uiOutput(ns("ui_phylotree_map"))
        )
      ),
      #  _____ tab:  sunburst
      
      tabPanel( 
        "Sunburst", 
        fluidRow(
          box(
            width = 4,
            status = "primary",
            solidHeader = TRUE,
            fluidRow(
              column(6,
                     uiOutput(ns("ui_sunburst_index"))
                     
              ),
              column(6
                     #,
                     
              ))
            ,
            
            fluidRow(
              column(6),
              column(6,
                     actionButton(ns("button_plot_sunburst"), 
                                  icon = icon("paper-plane"),
                                  label = "Plot",
                                  #style="float:right"
                                  style="float:right; color: #fff; background-color: #337ab7; border-color: #2e6da4"
                     )
              )
              
            )
          ),
          
          uiOutput(ns("ui_sunburst"))
          
          
        )
      ),
      #  _____ tab:  interactive hirarchical
      
      tabPanel( 
        "Interactive hirarchical plot", 
        fluidRow(
          box(
            width = 4,
            status = "primary",
            solidHeader = TRUE,
            fluidRow(
              column(6,
                     uiOutput(ns("ui_d3partitionR_index")),
                     radioButtons(ns("d3partitionR_plottype"),
                                  "Choose plot type",
                                  c("Interactive sunbust"='sunburst',
                                    "MetaMap circle tree"='circle_treemap',
                                    #"Classical treemap" = 'treemap',
                                    "Horizontal Partition bar" = 'icicle',
                                    "Vertical Partition bar"= 'partition_chart'),
                                  selected = "circle_treemap"
                     )
                     
              ),
              column(6,
                     textInput(ns("d3partition_rootname"),
                               "Root name",
                               "Life tree"
                     ),
                     sliderInput(ns("d3partition_label_fontsize"),
                                 "Label font size",
                                 min = 0.1,
                                 max = 5,
                                 value = 2)
                     
              ))
            ,
            
            fluidRow(
              column(6),
              column(6,
                     actionButton(ns("button_apply_D3partitionR"), 
                                  icon = icon("paper-plane"),
                                  label = "Plot",
                                  #style="float:right"
                                  style="float:right; color: #fff; background-color: #337ab7; border-color: #2e6da4"
                     )
              )
              
            )
          ),
          
          uiOutput(ns("UI_viz_D3partitionR"))
          
        )
      ),
      #  _____ tab:  rectangle treemap
      
      tabPanel( 
        "Interactive Rectangle TreeMap", 
        fluidRow(
          box(
            width = 4,
            status = "primary",
            solidHeader = TRUE,
            fluidRow(
              column(6,
                     uiOutput(ns("ui_treemap_index")),
                     uiOutput(ns("ui_treemap_vcolor")),
                     textInput(ns("treemap_root_name"),
                               "Root name",
                               "Bacteria"
                     ),
                     selectInput(ns("treemap_color_type"),
                                 "How the rectangles are colored",
                                 c("by branch" = "index", "by level"= "depth")
                     )
                     
                     
              ),
              column(6,
                     selectInput(ns("treemap_algorithm"),
                                 "treemap algorithm:",
                                 c( "Good aspect ratios, but ignores the sorting orde"= "squarified",
                                    "Takes the sorting order into account while aspect ratios are still acceptable" = "pivotSize")
                     ),
                     h5("This plot is conficting with the phylo tree, whichever plot first will show up.")
              )
            ),
            
            fluidRow(
              column(6),
              column(6,
                     actionButton(ns("button_apply_rectangle_treemap"), 
                                  icon = icon("paper-plane"),
                                  label = "Plot",
                                  style="float:right; color: #fff; background-color: #337ab7; border-color: #2e6da4"
                     )
              )
              
            )# fluidRow ends
          ),# left box ends
          uiOutput(ns("ui_treemap_display"))
        )
      )
      #end of tab
    ),
    shinysky::busyIndicator(text = "Loading, please wait ...",
                            img = "ajaxloaderq.gif",
                            wait = 500) 
  )
}
PHYLOPLOT2 <- function(input, output, session, data, type = "lineage"){
  ns <- session$ns
  # install/load required libraries
  install.packages.auto(treemap) # for treemap
  install.packages.auto(d3treeR)
  install.packages.auto(data.tree)
  install.packages.auto(D3partitionR)
  install.packages.auto(sunburstR)
  install.packages.auto(dplyr)
  
  # take over the data
  data_frame_for_plot <- data
  
  
  #_____ rectangle treemap
  
  output$ui_treemap_index <- renderUI({
    
    column_names_df <- colnames(data_frame_for_plot)
    selectInput(ns("treemap_index"), 
                "Select the levels to include in the tree map",
                column_names_df[-length(column_names_df)],
                selected = column_names_df[-length(column_names_df)],
                multiple = TRUE
    )
    
  })
  output$ui_treemap_vcolor <- renderUI({
    selectInput(ns("treemap_vcolor"), 
                "Select the level as the principle color",
                colnames(data_frame_for_plot)
    )
    
  })
  
  
  observe({
    if(input$button_apply_rectangle_treemap > 0 ){
      
      isolate({
        tree_df<- as.data.frame(data_frame_for_plot) # just in case not a data.frame
        # plot the tree
        
        #print(colnames(tree_df))
        
        output$tree <- d3treeR::renderD3tree2({
          d3treeR::d3tree2(
            treemap(tree_df,
                    index = input$treemap_index,
                    vSize = "Intensity",
                    vColor = input$treemap_vcolor,
                    type = input$treemap_color_type,
                    title = "my title",
                    algorithm = input$treemap_algorithm,
                    sortID = "size"),
            height = "800px",
            #width = "80%",
            rootname = input$treemap_root_name
          )
        })
        
        # for ui output
        output$ui_treemap_display <- renderUI({
          box(
            width = 8,
            #height = "800px",
            status = "primary",
            solidHeader = TRUE,
            d3tree2Output(ns('tree'), height=800)
            
          )
          
        })
        
      })
    }
    
  })
  
  #_____ phylotree map
  observe({
    if(input$button_apply_phylotree_map >0 ){
      
      tree_df<- as.data.frame(data_frame_for_plot) # just in case not a data.frame
      
      # note that this data.frame' s last column is the intensity
      
      tree_df[tree_df == ""] <- " "
      tree_df[is.na(tree_df)]<- " "
      
      # generate the string for tree converstion
      tree_df$pathString = future_apply(tree_df[1:length(tree_df) -1], 1, function(x) paste(x, collapse = "/"))
      
      my_taxid_tree <- data.tree::as.Node(tree_df, pathDelimiter = "/") # data.tree format
      
      # convert to list first
      my_taxid_tree_list <- data.tree::ToListExplicit(my_taxid_tree, unname = TRUE)
      
      isolate({
        
        # as a classical tree
        #diagonalNetwork(List = my_taxid_tree_list, fontSize = 10, opacity = 0.9)
        
        switch(input$phylo_tree_type,
               "Diagonal" = {
                 output$phylotree_map <- networkD3::renderDiagonalNetwork({
                   # output$Diagonal_tree <- renderPlot({
                   # 
                   networkD3::diagonalNetwork(my_taxid_tree_list,
                                              fontSize = input$phylo_tree_fontsize,
                                              linkColour = input$phylo_tree_line_color,
                                              nodeColour = input$phylo_tree_node_color,
                                              nodeStroke = input$phylo_tree_node_outlinecolor,
                                              textColour = input$phylo_tree_text_color,
                                              opacity  = as.numeric(input$phylo_tree_opacity)
                   )
                   
                 })
                 
                 output$ui_phylotree_map <- renderUI({
                   box(
                     width = 8,
                     #height = "800px",
                     status = "primary",
                     solidHeader = TRUE,
                     diagonalNetworkOutput(ns('phylotree_map'),
                                           height = "1200")
                     
                   )
                 })
                 
               },
               "Radial" = {
                 
                 output$phylotree_map <- networkD3::renderRadialNetwork({
                   
                   networkD3::radialNetwork(my_taxid_tree_list,
                                            fontSize = input$phylo_tree_fontsize,
                                            linkColour = input$phylo_tree_line_color,
                                            nodeColour = input$phylo_tree_node_color,
                                            nodeStroke = input$phylo_tree_node_outlinecolor,
                                            textColour = input$phylo_tree_text_color,
                                            opacity  = as.numeric(input$phylo_tree_opacity)
                                            
                                            
                   )
                   
                 })
                 
                 output$ui_phylotree_map <- renderUI({
                   box(
                     width = 8,
                     status = "primary",
                     solidHeader = TRUE,
                     radialNetworkOutput(ns('phylotree_map'))
                     
                   )
                 })
                 
               }
        )
      })
      
      
      
      
    }
  })
  
  #_____ surnburst
  
  output$ui_sunburst_index <- renderUI({
    
    column_names_df <- colnames(data_frame_for_plot)
    
    colindex <- 1:(length(column_names_df)-1)
    names(colindex) <- column_names_df[-length(column_names_df)]
    
    selectInput(ns("sunburst_index"), 
                "Select the >=2 levels to plot the sunburst",
                colindex,
                selected = colindex,
                multiple = TRUE
    )
    
  })
  
  
  observe({
    if(input$button_plot_sunburst >0 ){
      
      isolate({
        tree_df<- as.data.frame(data_frame_for_plot) # just in case not a data.frame
        #tree_df <-  read.delim("www/phylo_peptide.txt", header = TRUE, row.names = 1)
        tree_df <- tree_df[1:200,]
        # note that this data.frame' s last column is the intensity
        
        # only keep the selected columns to plot
        try(tree_df <- tree_df[,c(as.numeric(input$sunburst_index),ncol(tree_df)), drop = FALSE])
        #tree_df <- tree_df[,c(1:5, ncol(tree_df)), drop = FALSE]
        
        # generate the string of hirachical
        pathString = future_apply(tree_df[1:(ncol(tree_df)-1)], 1, function(x) paste(x, collapse = "-"))
        
        tree_df_for_sunburst <- data.frame(pathString, intensity = tree_df[, ncol(tree_df)])    
        
        output$sunburst <- renderSunburst({
          sunburst(tree_df_for_sunburst,
                   count = TRUE
          )
        })
        
        
        output$ui_sunburst <- renderUI({
          box(
            width = 8,
            #height = "800px",
            status = "primary",
            solidHeader = TRUE,
            sunburstOutput(ns('sunburst'), height = "800px")
            
          )
          
        })
        
      })
    }
    
  })
  
  
  #_____ More hirarchical plot
  
  
  output$ui_d3partitionR_index <- renderUI({
    
    column_names_df <- colnames(data_frame_for_plot)
    
    selectInput(ns("d3partitionR_index"), 
                "Select the >=2 levels to plot the sunburst",
                column_names_df[-length(column_names_df)],
                selected = column_names_df[-length(column_names_df)],
                multiple = TRUE
    )
    
  })
  
  
  observe({
    if(input$button_apply_D3partitionR >0 ){
      
      isolate({
        tree_df<- as.data.table(data_frame_for_plot) # just in case not a data.frame
        
        # remove any na or empty cells
        tree_df[tree_df == ""] <- " "
        tree_df[is.na(tree_df)]<- " "
        names(tree_df)[length(tree_df)] <- "Intensity"
        
        
        #print(input$d3partitionR_index)
        output$viz_D3partitionR<-renderD3partitionR({
          
          D3partitionR() %>%
            add_data(tree_df,
                     count = 'Intensity',
                     steps=input$d3partitionR_index,
                     label='name',
                     tooltip=c('name','Intensity')
                     #color = 'Intensity',
            ) %>%
            add_title(input$d3partition_rootname,
                      style = 'font-size: 200%;'
            ) %>%
            set_chart_type(input$d3partitionR_plottype) %>%
            set_legend_parameters(zoom_subset = TRUE) %>%
            set_labels_parameters(visible = T, 
                                  cut_off = 3,
                                  style =  paste0("font-size: ",as.numeric(input$d3partition_label_fontsize*100),"%")
            ) %>%
            
            set_tooltip_parameters(visible=FALSE, 
                                   style='background-color:lightblue;',
                                   builder='table') %>% 
            plot()
          
        })
        
        output$UI_viz_D3partitionR <- renderUI({
          box(
            width = 8,
            #height = "800px",
            status = "primary",
            solidHeader = TRUE,
            D3partitionROutput(ns('viz_D3partitionR'),height=800)
            
          )
          
        })
        
      })
    }
    
  })
  
  
}








# For sankey plot -
# miniumum example code

# library(shiny)
# library(shinydashboard)
# source("shiny_modules_in_development.R")
# 
# ui <- dashboardPage(
#   dashboardHeader(title = "Dynamic sidebar"),
#   dashboardSidebar(
#     sidebarMenu(id="tabs",
#                 menuItem("Menu item1", tabName="m1", icon = icon("calendar"))
#     )
#   ),
#   dashboardBody(
#     tabItems(
#       tabItem(tabName = "m1", 
#               
#               fluidRow( # this fluidrow is to ensure the extend the height
#                 tabBox(
#                   id ="tabset_result_show_taxon",
#                   width = 12,
#                   tabPanel("sankey",
#                            SANKEY_UI("demo")
#                   )
#                 )
#               )
#       )
#     )
#   )
# )
# server <- function(input, output,session) {
#   # mat = matrix(sample(100,9), 3)
#   # rownames(mat) = letters[1:3]
#   # colnames(mat) = LETTERS[1:3]
#   mat <- as.matrix(read.delim("taxon_function_table.txt", row.names = 1))
#   mat[mat < 3] <-0
#   #mat
#   
#   observe({
#     callModule(SANKEY, "demo", data = mat, type = "matrix")
#   })
# }
# shinyApp(ui, server)
# 
# 




SANKEY_UI <-function(id){
  ns <- NS(id)
  fluidRow(
    box(
      width = 4,
      status = "primary",
      solidHeader = TRUE,
      fluidRow(
        column(6,
               checkboxInput(ns("simple_data_process"),
                             "Process data before plotting?",
                             FALSE
               )
        )
      ),
      
      conditionalPanel(
        condition = paste0("input['", ns("simple_data_process"), "']"),
        fluidRow(
          column(6,
                 selectInput(ns("simple_Log_transformation"),
                             "Log transformation",
                             c("no transform","log2", "log10", "-log10")
                 )
          ),
          column(6
          )
        ),
        fluidRow(
          column(6,
                 checkboxInput(ns("Filter_out_values"),
                               "Filter out values",
                               FALSE
                 )
          )
        ),
        
        conditionalPanel(
          condition = paste0("input['", ns("Filter_out_values"), "']"),
          fluidRow(
            column(6,
                   wellPanel(
                     selectInput(ns("filter_condition"),
                                 "Filter condtion",
                                 c("<=", "<", ">=", ">")
                     )
                     
                   )
                   
            ),
            column(6,
                   wellPanel(
                     textInput(ns("Filter_value"),
                               "Filter by value",
                               "0"
                     )
                   )
                   
            )
          )
        )
        
      ),
      fluidRow(
        column(6,
               uiOutput(ns("topN_to_plot")),
               
               
               checkboxInput(ns("sankey_grouping_with_color"),
                             "Color Goups?",
                             FALSE
               ),
               
               selectInput(ns("Sankey_color_scheme"),
                           "Color theme:",
                           c("d3.schemeCategory10" =   "d3.scaleOrdinal(d3.schemeCategory10);",
                             "d3.schemeCategory20" =   "d3.scaleOrdinal(d3.schemeCategory20);",
                             "d3.schemeCategory20b" = "d3.scaleOrdinal(d3.schemeCategory20b);",
                             "d3.schemeCategory20c" = "d3.scaleOrdinal(d3.schemeCategory20c);"
                           )
                           
               ),
               sliderInput(ns("sankey_node_padding"),
                           "Node Padding",
                           min = 1,
                           max = 40,
                           value = 10
               ),
               
               sliderInput(ns("sankey_node_width"),
                           "Node width",
                           min = 1,
                           max = 100,
                           value = 30
               )
        ),
        column(6,
               checkboxInput(ns("sankey_range_scaling"),
                             "Scale the value to 1:100?",
                             FALSE
               ),
               
               sliderInput(ns("sankey_margin"),
                           "Plot margin",
                           min = 1,
                           max = 40,
                           value = 10
               )
               ,
               sliderInput(ns("sankey_font_size"),
                           "Font size",
                           min = 1,
                           max = 40,
                           value = 20
               )
               ,
               sliderInput(ns("sankey_height"),
                           "Plot Height",
                           min = 400,
                           max = 1000,
                           value = 600
               ),
               checkboxInput(ns("sankey_sink_right"),
                             "Sink Right?",
                             TRUE
               )
               
        )
        
        
      ),
      
      fluidRow(
        column(6),
        column(6,
               actionButton(ns("button_apply_sankeyplot"), 
                            icon = icon("paper-plane"),
                            label = "Plot",
                            style="float:right; color: #fff; background-color: #337ab7; border-color: #2e6da4"
               )
        )
        
      )
    ),
    uiOutput(ns("ui_sankey_plot"))
  )
}

# data could be ajacent matrix or list

SANKEY <- function(input, output, session, data, type = "matrix", meta = NULL){
  ns <- session$ns
  install.packages.auto(networkD3) # for sankey network plot
  
  if(type == "list" && ncol(data) != 3){
    print("list data has to be with three columns")
  } 
  
  # take over the input, convert to list/data.frame if it is a matrix 
  df_edge <- switch(type,
                    "matrix" = {
                      data <- as.matrix(data)
                      reshape2::melt(as.matrix(data))
                    },
                    "list" = {data}
                    
  )
  # change the column names for easy mapping
  try(colnames(df_edge) <- c("from", "to", "value"))
  
  output$topN_to_plot <- renderUI({
    numericInput(ns("sankey_number"),
                 "Top N links to plot",
                 min = 1,
                 max = nrow(df_edge),
                 value = 10
    )
  })
  
  
  
  observe({
    
    # do log if use set, on data.frame level
    switch(input$simple_Log_transformation,
           "no transform" = data,
           "log2" = {
             df_edge[,3] <- log2(df_edge[,3])
             df_edge <- df_edge[-which(is.infinite(df_edge[,3])),] # remove the rows with infinite value
           },
           "log10" = {
             df_edge[,3] <- log10(df_edge[,3])
             df_edge <- df_edge[-which(is.infinite(df_edge[,3])),]
           },
           "-log10" = {
             df_edge[,3] <- -log10(df_edge[,3])
             df_edge <- df_edge[-which(is.infinite(df_edge[,3])),]
           }
    )
    
    # filtering values
    if(input$Filter_out_values){
      switch(input$filter_condition,
             "<=" = { df_edge <- df_edge[-which(df_edge[,3] <= as.numeric(input$Filter_value)),] },
             "<"  = { df_edge <- df_edge[-which(df_edge[,3] <  as.numeric(input$Filter_value)),] },
             ">=" = { df_edge <- df_edge[-which(df_edge[,3] >= as.numeric(input$Filter_value)),] },
             ">"  = { df_edge <- df_edge[-which(df_edge[,3] >  as.numeric(input$Filter_value)),] }
      )
    }
    
    if(input$button_apply_sankeyplot > 0 ){
      isolate({
        topN <- as.numeric(input$sankey_number)
        # only keep topN
        if(nrow(df_edge) > topN){
          # reorder
          df_edge_filtered <- df_edge[order(df_edge[,3],decreasing = TRUE), ]
          # take the topN
          df_edge_for_sankey <- df_edge_filtered[1:topN,]
        }else{
          df_edge_for_sankey <- df_edge
        }
        
        # re scaling 
        if(input$sankey_range_scaling){
          df_edge_for_sankey[,3] <- range_standarize_100(df_edge_for_sankey[,3])
        }
        # convert data.frame into a sankey compatible format                                                                                                                                                      the 
        result_for_sankey <- recode_for_sankey(df_edge_for_sankey)
        
        
        
        if(input$sankey_grouping_with_color){
          # get the meta from the ajacent table or from user input for list data.frame
          my_meta <- switch (type,
                             "matrix" = {
                               df1 <- data.frame(sample = colnames(data),group = "column")
                               df2 <- data.frame(sample = rownames(data),group = "row")
                               rbind.data.frame(df1, df2)
                             },
                             "list" = {
                               meta
                             }
          )
          
          # attach the grouping information to the node data.frame
          result_for_sankey$df_nodes$group  <- my_meta$group[match(result_for_sankey$df_nodes$name,my_meta$sample)]
          
          # plot
          output$sankeyplot <- renderSankeyNetwork({
            sankeyNetwork(Links = result_for_sankey$df_links, 
                          Nodes = result_for_sankey$df_nodes,
                          Source = "from", 
                          Target = "to",
                          Value = "value", 
                          NodeID = "name",
                          NodeGroup = "group",
                          nodePadding = input$sankey_node_padding,
                          margin = input$sankey_margin,
                          fontSize= input$sankey_font_size, 
                          nodeWidth = input$sankey_node_width,
                          sinksRight = input$sankey_sink_right,
                          colourScale = JS(input$Sankey_color_scheme)
            )
          })
        }else{
          output$sankeyplot <- renderSankeyNetwork({
            sankeyNetwork(Links = result_for_sankey$df_links, 
                          Nodes = result_for_sankey$df_nodes,
                          Source = "from", 
                          Target = "to",
                          Value = "value", 
                          NodeID = "name",
                          nodePadding = input$sankey_node_padding,
                          margin = input$sankey_margin,
                          fontSize= input$sankey_font_size,
                          nodeWidth = input$sankey_node_width,
                          sinksRight = input$sankey_sink_right,
                          colourScale = JS(input$Sankey_color_scheme)
            )
          })
          
        }
        
        
        output$ui_sankey_plot <- renderUI({
          box(
            width = 8,
            status = "primary",
            solidHeader = TRUE,
            sankeyNetworkOutput(ns("sankeyplot"),
                                height = paste0(input$sankey_height,"px"))
          )
        })
      })
    }
  })
}



# For circos plot from adjacency matrix--
# demo file: module_contigency_cricos_plot_usemodule.R


ADJACENCY_MATRIX_LIST_CIRCOS_UI <-function(id){
  ns <- NS(id)
  fluidRow(
    box(
      width = 4,
      solidHeader = TRUE,
      status = "primary",
      
      fluidRow(
        column(6,
               uiOutput(ns("ui_topN_to_plot")),
               numericInput(ns("chorddiagram_general_gap"),
                            "Gap size between elements",
                            value = 1,
                            min = 1,
                            max = 50,
                            step = 1,
                            width = "200px"
                            
               ),
               numericInput(ns("chorddiagram_groups_gap"),
                            "Gap size between groups",
                            value = 10,
                            min = 1,
                            max = 50,
                            step = 1,
                            width = "200px"
                            
               ),
               
               numericInput(ns("chorddiagram_Rotation_degree"),
                            "Rotate from the original point(right middle/3 O'clock)",
                            value = 0,
                            min = 0,
                            max = 360,
                            step = 1,
                            width = "200px"
                            
               ),
               checkboxInput(ns("chorddiagram_plot_direction"),
                             "Plot element by clockwise direction?",
                             TRUE
               ),
               selectInput(ns("node_color_selection"),
                           "Node colors",
                           c("auto", "customize")
               )
               
               
        ),
        column(6,
               
               textInput(ns("chorddiagram_maintitle"),
                         "Main title:",
                         ""
               ),
               selectInput(ns("chorddiagram_annotation"),
                           "Labeling/Annotation tracks",
                           c("name", "grid", "axis"),
                           selected = c("name", "grid"),
                           multiple = TRUE
               ),
               checkboxInput(ns("chorddiagram_vertical_labeling"),
                             "Vertical Labeling?",
                             FALSE
               ),
               
               sliderInput(ns("chorddiagram_link_transparency"),
                           "Link color transparency",
                           min = 0,
                           max = 1,
                           value = 0.5
               ),
               selectInput(ns("chorddiagram_map_link_color"),
                           "Link colors:",
                           c("Follow Node colors", "values"),
                           selected = NULL
               ),
               conditionalPanel(
                 condition = paste0("input['", ns("chorddiagram_map_link_color"), "']", "=='values'"),
                 
                 colourInput(
                   ns("Value_low_color"), 
                   "Low colour", 
                   "#FFEEEE",
                   #showColour = "background",
                   palette = "limited"
                 ),
                 colourInput(
                   ns("Value_high_color"), 
                   "High color", 
                   "#FF0000",
                   #showColour = "background",
                   palette = "limited"
                 )
               )
               
               
        )
      ),
      
      conditionalPanel(
        condition = paste0("input['", ns("node_color_selection"), "']", "=='customize'"),
        fluidRow(
          column(12,
                 useColor_UI(ns("circos_node_colors"),
                             initialize_show = TRUE,
                             caption = "Show/hide node color options",
                             method = "RcolorBrewer",
                             Rcolorbrewer_schemes = "div",
                             RcolorBrewer_theme = "BrBG",
                             ncolors = 50)
          )
        )
      ),
      
      
      fluidRow(
        column(6),
        column(6,
               actionButton(ns("button_plot_circos"), 
                            icon = icon("paper-plane"),
                            label = "Plot",
                            #style="float:right"
                            style="float:right; color: #fff; background-color: #337ab7; border-color: #2e6da4"
               )
        )
      )  
    ),
    uiOutput(ns("ui_circos_plot"))
  )
}



# type = "matrix" or "list"
# meta can be from ajacent dimentions or from given meta information
ADJACENCY_MATRIX_LIST_CIRCOS <- function(input, output, session, data, type = "matrix", meta = NULL){
  ns <- session$ns
  install.packages.auto(circlize) # for circos plot
  # take over the ouput, convert to list if it is a a matrix 
  
  data_list <- switch(type,
                      "matrix" = {
                        data <- as.matrix(data)
                        # just in case there is no colnames or row names
                        if(is.null(colnames(data))){
                          colnames(data) <- paste0("col_", 1: ncol(data))
                        }
                          
                        if(is.null(rownames(data))){
                          rownames(data) <- paste0("col_", 1: nrow(data))
                        }

                        reshape2::melt(data) # not that melting matrix and data.frame are different
                      },
                      "list" = {data}
                      
  )
  # order
  data_list <- data_list[order(data_list[[3]],decreasing = TRUE),]
  
  
  # remove zero ones, otherwise not able to plot
  data_list <- data_list[data_list[[3]] > 0,]
  
  
  if(nrow(data_list) > 200){
    topN_to_plot <- 200
  }else{
    topN_to_plot <- nrow(data_list)
  }
  
  
  output$ui_topN_to_plot <- renderUI({
    numericInput(ns("chorddiagram_number"),
                "TopN (quality) connections to plot",
                min = 1,
                max = topN_to_plot,
                value = 10
    )
    
  })
  
  
  
  my_meta <- switch (type,
                     "matrix" = {
                       df1 <- data.frame(sample = colnames(data),group = "column")
                       df2 <- data.frame(sample = rownames(data),group = "row")
                       rbind.data.frame(df1, df2)
                     },
                     "list" = {
                       meta
                     }
  )
  
  
  mycol_node <- callModule(useColor, "circos_node_colors")
  #print(mycol_node())
  
  observe({
    topN <- as.numeric(isolate(input$chorddiagram_number))
    
    
    if(input$button_plot_circos > 0){
      
      
      # only keep the topN 
      if(nrow(data_list) > topN){
        df_edge_for_circos <- data_list[1:topN,]
      }else{
        df_edge_for_circos <- data_list
      }
      
      unique_nodes <- union(df_edge_for_circos[,1],df_edge_for_circos[,2])
      colorbrewer_colors <- rownames(brewer.pal.info[brewer.pal.info$category == "qual",])
      # this if no
      if(!is.null(my_meta)){
        
        my_meta_filtered <- my_meta[match(unique_nodes, my_meta[,1]),] # get the grouping information
        my_meta_filtered <- my_meta_filtered[order(my_meta_filtered[[2]]),] # reorder according to the groups
        
        groups_tab <- table(my_meta_filtered[,2])
        gaps_index <- cumsum(groups_tab)
        
        plot_order <- my_meta_filtered[,1] # set the plot order 
        # # set the gaps between groups
        gaps <-rep(input$chorddiagram_general_gap, length(unique_nodes))
        gaps[gaps_index] <- input$chorddiagram_groups_gap
        
        if(input$node_color_selection =="auto"){
          # get colors
          # get the colorbrewer colors, here using quality panel
          
          
          grid_colors <- c()
          # assgin colors using the same order, differnt groups with differn color theme
          for(i in 1: length(groups_tab)){
            grid_colors <-  c(grid_colors, colorRampPalette(brewer.pal(8,colorbrewer_colors[i]))(groups_tab[i]))
          }
        }else{
          
          if(length(unique_nodes) < length(mycol_node())){
            grid_colors <- mycol_node()[1:length(unique_nodes)]
          }else{
            grid_colors <- rep(mycol_node(),ceiling(length(unique_nodes)/length(mycol_node()))) [1:length(unique_nodes)]
          }
          
        }
        
      }else{
        
        gaps <-rep(input$chorddiagram_general_gap, length(unique_nodes))
        plot_order <-  my_meta[,1]
        
        if(input$node_color_selection =="auto"){
          # get colors
          # get the colorbrewer colors, here using quality panel
          grid_colors <-  colorRampPalette(brewer.pal(8,colorbrewer_colors[1]))(length(unique_nodes))
        }else{
          
          if(length(unique_nodes) < length(mycol_node())){
            grid_colors <- mycol_node()[1:length(unique_nodes)]
          }else{
            grid_colors <- rep(mycol_node(),ceiling(length(unique_nodes)/length(mycol_node()))) [1:length(unique_nodes)]
          }
          
        }
      }
      
      
      # set the link colors using the correlation mapping
      col_fun <- switch(input$chorddiagram_map_link_color,
                        "Follow Node colors" = {NULL},
                        "values" = {
                          colorRamp2(range(df_edge_for_circos[[3]]), 
                                     c(input$Value_low_color, input$Value_high_color), 
                                     transparency = input$chorddiagram_link_transparency)
                          
                        }
                        
      )
      
      
      
      isolate({
        
        # plot the chorddiagram
        svg(tempfile())  
        dev.control('enable')  
        
        #circos.par(gap.after = gaps, start.degree = 90, clock.wise = TRUE)
        circos.clear()
        
        circos.par(gap.degree = as.numeric(gaps), 
                   start.degree = as.numeric(input$chorddiagram_Rotation_degree), 
                   clock.wise = input$chorddiagram_plot_direction)
        
        if(input$chorddiagram_vertical_labeling){
          chordDiagram(df_edge_for_circos, 
                       grid.col = grid_colors,
                       annotationTrack = "grid",
                       preAllocateTracks = list(track.height = max(strwidth(unique_nodes))),
                       col = col_fun,
                       transparency = input$chorddiagram_link_transparency,
                       order = plot_order)
          
          # we go back to the first track and customize sector labels
          circos.track(track.index = 1, panel.fun = function(x, y) {
            circos.text(CELL_META$xcenter, CELL_META$ylim[1], CELL_META$sector.index,
                        facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5))
          }, bg.border = NA) # here set bg.border to NA is important
          
          
        }else{
          chordDiagram(df_edge_for_circos, 
                       grid.col = grid_colors,
                       col = col_fun,
                       transparency = as.numeric(input$chorddiagram_link_transparency),
                       order = plot_order,
                       annotationTrack = input$chorddiagram_annotation
          )
          
        }
        
        title(input$chorddiagram_maintitle)
        #chordDiagram(df_edge_for_circos)
        circos.clear()
        
        plot_circos <- recordPlot()
        dev.off()
        
        
        
        callModule(plotInBox, "plot_circos",
                   plot_object = plot_circos,
                   name_tag = "plot_circos",
                   plot_type = "recordPlot")
        
        output$ui_circos_plot <- renderUI({
          plotInBox_UI(ns("plot_circos"), boxwidth = 8)
        })
        
      })
    }
    
  })
}







# for data_matrix profiling

# minimum example
# library(shiny)
# library(shinydashboard)
# source("shiny_modules_in_development.R")

# ui <- dashboardPage(
#   dashboardHeader(title = "Dynamic sidebar"),
#   dashboardSidebar(
#     sidebarMenu(id="tabs",
#                 menuItem("Menu item1", tabName="m1", icon = icon("calendar"))
#     )
#   ),
#   dashboardBody(
#     tabItems(
#       tabItem(tabName = "m1", 
#               
#               fluidRow( # this fluidrow is to ensure the extend the height
#                 tabBox(
#                   id ="tabset_result_show_taxon",
#                   width = 12,
#                   tabPanel("heatmap module tab",
#                            MATRIX_PROFILE_UI("demo")
#                   )
#                 )
#               )
#       )
#     )
#   )
# )
# server <- function(input, output,session) {
#   matched_hits  <- matrix(sample(1:1000, 200), nrow = 20)
#   observe({
#     callModule(MATRIX_PROFILE, "demo", data_matrix = matched_hits)
#   })
# }
# shinyApp(ui, server)
# 




MATRIX_PROFILE_UI <-function(id){
  ns <- NS(id)
  fluidRow(
    box(
      width = 4,
      solidHeader = TRUE,
      status = "primary",
      fluidRow(
        column(6,
               checkboxInput(ns("simple_data_process"),
                             "Process data before plotting?",
                             FALSE
               )
        )
      ),
      
      conditionalPanel(
        condition = paste0("input['", ns("simple_data_process"), "']"),
        fluidRow(
          column(6,
                 selectInput(ns("Log_transformation"),
                             "Log transformation",
                             c("no transform","log2", "log10", "-log10")
                 )
          ),
          column(6,
                 checkboxInput(ns("transpose_matrix"),
                               "Transpose matrix?",
                               FALSE
                 )
          )
        ),
        fluidRow(
          column(6,
                 checkboxInput(ns("Filter_out_values"),
                               "Filter out values",
                               FALSE
                 )
          )
        ),
        
        conditionalPanel(
          condition = paste0("input['", ns("Filter_out_values"), "']"),
          fluidRow(
            column(6,
                   wellPanel(
                     selectInput(ns("filter_condition"),
                                 "Filter condtion",
                                 c("<=", "<", ">=", ">")
                     ),
                     textInput(ns("Filter_value"),
                               "Filter by value",
                               "0"
                     )
                   )
                   
            ),
            column(6,
                   wellPanel(
                     textInput(ns("valid_value_number_row"),
                               "Number of valid values for each row",
                               "1"
                     ),
                     textInput(ns("valid_value_number_col"),
                               "Number of valid values for each column",
                               "1"
                     )
                   )
                   
            )
          )
        )
        
      ),
      
      fluidRow(
        column(6,
               selectInput(ns("correlation_plot_method"),
                           "Choose the shape to visualize:",
                           c("circle", "square", "ellipse", "number", "shade",
                             "color", "pie"),
                           selected = "pie"
               )
               #,
               
               # checkboxInput(ns("correlation_hits_log10"),
               #               "log10 transform?", TRUE
               # )
               
        ),
        column(6,
               numericInput(ns("correlation_plot_label_size"),
                            "Text label size",
                            max = 10,
                            min = 0.1,
                            step = 0.1,
                            value = 0.8,
                            width = "200px"
               ),
               numericInput(ns("correlation_plot_label_tilt"),
                            "Text label tilt degree ",
                            step = 1,
                            value = 30,
                            width = "200px"
               )
               
        )
      ),
      fluidRow(
        column(12,
               useColor_UI(ns("correlation_matrix_color"),
                           initialize_show = TRUE,
                           method = "RcolorBrewer",
                           Rcolorbrewer_schemes = "div",
                           RcolorBrewer_theme = "BrBG",
                           ncolors = 100)
        )
        
        
      ),
      fluidRow(
        column(6),
        column(6,
               actionButton(ns("button_plot_taxon_profile"), 
                            icon = icon("paper-plane"),
                            label = "Plot",
                            #style="float:right"
                            style="float:right; color: #fff; background-color: #337ab7; border-color: #2e6da4"
               )
        )
      )  
    ),
    uiOutput(ns("ui_taxon_profile_plot"))
  )
}



MATRIX_PROFILE <- function(input, output, session, data_matrix){
  ns <- session$ns
  install.packages.auto(corrplot)
  # take over the ouput 
  t <- data_matrix
  
  mycol_correlation <- callModule(useColor, "correlation_matrix_color")
  
  observe({
    
    t <- switch(input$Log_transformation,
                "no transform" = t,
                "log2" = {
                  t <- log2(t)
                  t[is.infinite(t)] <- 0
                  t
                },
                "log10" = {
                  t<-log10(t)
                  t[is.infinite(t)] <- 0
                  t
                },
                "-log10" = {
                  t <- -log10(t)
                  t[is.infinite(t)] <- 0
                  t
                }
                
    )
    if(input$transpose_matrix){
      t <-t(t)
    }
    
    #
    
    if(input$Filter_out_values){
      t <- switch(input$filter_condition,
                  "<=" = {
                    t[t <= as.numeric(input$Filter_value)] <- 0
                    t[which((ncol(t) -rowSums(t == 0)) >= as.numeric(input$valid_value_number_row)),  
                      which((nrow(t) -colSums(t == 0)) >= as.numeric(input$valid_value_number_col))]
                  },
                  "<" = {
                    t[t < as.numeric(input$Filter_value)] <- 0
                    t[which((ncol(t) -rowSums(t == 0)) >= as.numeric(input$valid_value_number_row)),  
                      which((nrow(t) -colSums(t == 0)) >= as.numeric(input$valid_value_number_col))]
                  },
                  
                  ">=" = {
                    t[t >= as.numeric(input$Filter_value)] <- 0
                    t[which((ncol(t) -rowSums(t == 0)) >= as.numeric(input$valid_value_number_row)),  
                      which((nrow(t) -colSums(t == 0)) >= as.numeric(input$valid_value_number_col))]
                    
                  },
                  ">" = {
                    t[t > as.numeric(input$Filter_value)] <- 0
                    t[which((ncol(t) -rowSums(t == 0)) >= as.numeric(input$valid_value_number_row)),  
                      which((nrow(t) -colSums(t == 0)) >= as.numeric(input$valid_value_number_col))]
                    
                  }
                  
      )
    }
    
    
    if(input$button_plot_taxon_profile > 0){
      
      isolate({
        svg(tempfile())
        dev.control('enable')
        corrplot(t,
                 type = "full", # has to be full for a profile display
                 is.corr = FALSE,
                 col = mycol_correlation(),
                 tl.col = "black",
                 tl.cex = input$correlation_plot_label_size,
                 tl.srt = input$correlation_plot_label_tilt,
                 title = "",
                 method =input$correlation_plot_method,
                 addgrid.col =NA,
                 outline = FALSE)
        
        p_correlation <- recordPlot()
        dev.off()
        
        callModule(plotInBox, "corrplot",
                   plot_object = p_correlation,
                   name_tag = "plot_correlation_filtered",
                   plot_type = "recordPlot")
        
        output$ui_taxon_profile_plot <- renderUI({
          plotInBox_UI(ns("corrplot"), boxwidth = 8)
        })
        
      })
    }
    
  })
}




# for enrichment profile display
# accepts two corresponding data matrixs of matched hits annd pvalues

# minimum example:


# library(shiny)
# library(shinydashboard)
# source("shiny_modules_in_development.R")
# 
# ui <- dashboardPage(
#   dashboardHeader(title = "Dynamic sidebar"),
#   dashboardSidebar(
#     sidebarMenu(id="tabs",
#                 menuItem("Menu item1", tabName="m1", icon = icon("calendar"))
#     )
#   ),
#   dashboardBody(
#     tabItems(
#       tabItem(tabName = "m1",
# 
#               fluidRow( # this fluidrow is to ensure the extend the height
#                 tabBox(
#                   id ="tabset_result_show_taxon",
#                   width = 12,
#                   tabPanel("heatmap module tab",
#                            ENRICH_PROFILE_UI("demo")
#                   )
#                 )
#               )
#       )
#     )
#   )
# )
# server <- function(input, output,session) {
# 
#   matched_hits  <- matrix(sample(1:1000, 200), nrow = 20)
#   matched_pvalue  <- matrix(sample(1:10000, 200)/10000, nrow = 20)
#   observe({
#     callModule(ENRICH_PROFILE, "demo", matched_hits = matched_hits, matched_pvalue = matched_pvalue)
#   })
# 
# 
# }
# shinyApp(ui, server)
# 
# 



ENRICH_PROFILE_UI <-function(id){
  ns <- NS(id)
  fluidRow(
    box(
      width = 4,
      solidHeader = TRUE,
      status = "primary",
      fluidRow(
        column(12,
               selectInput(ns("correlation_plot_data"),
                           "Choose what to plot:",
                           c("Matched Hits", "P-value", "Matched Hits with p-value filtering"),
                           selected = "Matched Hits with p-value filtering"
               )
        )
        
      ),
      fluidRow(
        column(6,
               
               
               
               selectInput(ns("correlation_plot_method"),
                           "Choose the shape to visualize:",
                           c("circle", "square", "ellipse", "number", "shade",
                             "color", "pie"),
                           selected = "pie"
               ),
               
               conditionalPanel(
                 condition = paste0("input['", ns("correlation_plot_data"), "']", "=='Matched Hits'"),
                 checkboxInput(ns("correlation_hits_log10"),
                               "log10 transform?", TRUE
                 )
               ),
               
               conditionalPanel(
                 condition = paste0("input['", ns("correlation_plot_data"), "']", "=='P-value'"),
                 checkboxInput(ns("correlation_pvalue_log10"),
                               "-log10 transform?", TRUE
                 )
               ),
               
               conditionalPanel(
                 condition = paste0("input['", ns("correlation_plot_data"), "']", "=='Matched Hits with p-value filtering'"),
                 numericInput(ns("correlation_plot_pvalue_cutoff"),
                              "P-value cutoff",
                              max = 1,
                              value = 0.01,
                              width = "200px"
                 ),
                 selectInput(ns("correlation_plot_insig_behavor"),
                             "Behavior for the insignificant nodes",
                             c("pch", "p-value", "blank", "n", "label_sig"),
                             selected = "blank"
                 )
               )
               
        ),
        column(6,
               numericInput(ns("correlation_plot_label_size"),
                            "Text label size",
                            max = 10,
                            min = 0.1,
                            step = 0.1,
                            value = 0.8,
                            width = "200px"
               ),
               numericInput(ns("correlation_plot_label_tilt"),
                            "Text label tilt degree ",
                            step = 1,
                            value = 30,
                            width = "200px"
               )
               
        )
      ),
      fluidRow(
        column(12,
               useColor_UI(ns("correlation_matrix_color"),
                           initialize_show = TRUE,
                           method = "RcolorBrewer",
                           Rcolorbrewer_schemes = "div",
                           RcolorBrewer_theme = "BrBG",
                           ncolors = 100)
        )
        
        
      ),
      fluidRow(
        column(6),
        column(6,
               actionButton(ns("button_plot_taxon_profile"), 
                            icon = icon("paper-plane"),
                            label = "Plot",
                            #style="float:right"
                            style="float:right; color: #fff; background-color: #337ab7; border-color: #2e6da4"
               )
        )
      )  
    ),
    uiOutput(ns("ui_taxon_profile_plot"))
  )
  
  
}



ENRICH_PROFILE <- function(input, output, session, matched_hits, matched_pvalue){
  ns <- session$ns
  install.packages.auto(corrplot)
  # take over the ouput 
  
  
  x <- matched_hits
  p <- matched_pvalue
  
  mycol_correlation <- callModule(useColor, "correlation_matrix_color")
  
  observe({
    
    if(input$correlation_hits_log10){
      x <- log10(x)
      x[is.infinite(x)] <- NA
      x[is.na(x)] <- 0
    }
    
    if(input$correlation_pvalue_log10){
      p_log <- -log10(p)
      p_log[is.infinite(p_log)] <- NA
    }else{
      p_log <- p
    }
    
    
    if(input$button_plot_taxon_profile > 0){
      isolate({
        switch(input$correlation_plot_data,
               "Matched Hits with p-value filtering" = {
                 svg(tempfile())
                 dev.control('enable')
                 corrplot(x, 
                          p.mat = p, 
                          type = "full", # has to be full for a profile display
                          is.corr = FALSE, 
                          col = mycol_correlation(),
                          tl.col = "black",
                          tl.cex = input$correlation_plot_label_size,
                          tl.srt = input$correlation_plot_label_tilt,
                          title = "",
                          insig = input$correlation_plot_insig_behavor,
                          sig.level = as.numeric(input$correlation_plot_pvalue_cutoff),
                          method =input$correlation_plot_method,
                          addgrid.col =NA, 
                          outline = FALSE)
                 
                 p_correlation <- recordPlot()
                 dev.off()
                 
               },
               "P-value" = {
                 
                 #x[is.na(p_log)] <- max(p_log, na.rm = TRUE)
                 
                 svg(tempfile())
                 dev.control('enable')
                 corrplot(p_log, 
                          type = "full", # has to be full for a profile display
                          is.corr = FALSE, 
                          col = mycol_correlation(),
                          tl.col = "black",
                          tl.cex = input$correlation_plot_label_size,
                          tl.srt = input$correlation_plot_label_tilt,
                          title = "",
                          method =input$correlation_plot_method,
                          addgrid.col =NA, 
                          outline = FALSE)
                 
                 p_correlation <- recordPlot()
                 dev.off()
               },
               "Matched Hits" = {
                 svg(tempfile())
                 dev.control('enable')
                 corrplot(x, 
                          type = "full", # has to be full for a profile display
                          is.corr = FALSE, 
                          col = mycol_correlation(),
                          tl.col = "black",
                          tl.cex = input$correlation_plot_label_size,
                          tl.srt = input$correlation_plot_label_tilt,
                          title = "",
                          method =input$correlation_plot_method,
                          addgrid.col =NA, 
                          outline = FALSE)
                 
                 p_correlation <- recordPlot()
                 dev.off()
                 
               }
        )
        
        
        callModule(plotInBox, "corrplot",
                   plot_object = p_correlation,
                   name_tag = "plot_correlation_filtered",
                   plot_type = "recordPlot")
        
        
        
        output$ui_taxon_profile_plot <- renderUI({
          plotInBox_UI(ns("corrplot"), boxwidth = 8)
        })
        
      })
    }
    
    
    
    
  })
}


# accept a data.matrix or data.frame
# ui output is a box, can be placed into a column to fit the width

### minimum sample

# library(shiny)
# library(shinydashboard)
# source("shiny_modules_in_development.R")
# 
# ui <- dashboardBody(
#   fluidPage(
#     fluidRow(
#       column(3,
#              box(
#                width = 12,
#                status = "primary",
#                solidHeader = TRUE,
#                radioButtons("test_data_select", "test",c("data1","data2"))
#              )
#              
#       ),
#       column(9,
#              DATATABLE_Display_UI("demo")
#       )
#       
#     )
#   )
# )
# server <- function(input, output,session) {
#   
#   d1 <- iris
#   d2 <- mtcars
#   
#   observe({
#     switch(input$test_data_select,
#            "data1" = {callModule(DATATABLE_Display, "demo", data_table = d1, filename_tag = "test")},
#            "data2" = {callModule(DATATABLE_Display, "demo", data_table = d2, filename_tag = "test")}
#     )
#   })
#   
# }
# shinyApp(ui, server)
# 







DATATABLE_Display_UI <- function(id, boxtitle = NULL,boxwidth = 12){
  ns <- NS(id)
  box(
    width = boxwidth,
    status = "primary",
    title = boxtitle,
    solidHeader = TRUE,
    tableDownloadUI(ns("download_table"), 
                    label = "Download table"),
    DT::dataTableOutput(ns('table_output_data_for_network'))
  )
  
}



DATATABLE_Display <- function(input, output, session, data_table, filename_tag, height = 600){
  ns <- session$ns
  install.packages.auto(DT) # for table 
  callModule(tableDownload,"download_table", 
             data = data_table, 
             filename_tag = filename_tag) 
  
  output$table_output_data_for_network <- DT::renderDataTable(
    data_table,
    filter = 'top',
    extensions = c('Scroller'),
    options = list(
      autoWidth = TRUE,
      pageLength = 50,
      dom = 'Brtip',
      #buttons = c('colvis'),
      scrollY = height,
      scrollX = TRUE)
  )
  
}

DATATABLE_Display_SIMPLE_UI <- function(id){
  ns <- NS(id)
  DT::dataTableOutput(ns('table_output_data_for_network'))
  
}

DATATABLE_Display_SIMPLE <- function(input, output, session, data_table, height = 600){
  ns <- session$ns
  install.packages.auto(DT) # for table 
  output$table_output_data_for_network <- DT::renderDataTable(
    data_table,
    filter = 'top',
    extensions = c('Scroller'),
    options = list(
      autoWidth = TRUE,
      pageLength = 50,
      dom = 'Brtip',
      #buttons = c('colvis'),
      scrollY = height,
      scrollX = TRUE)
  )
  
}



# mode for plotting bar plot, 

# ui does not take any extra parameter
# server function only takes one argument of data_matrix to plot
# 

# minimum example:

# library(shiny)
# library(shinydashboard)
# source("shiny_modules.R")
# 
# ui <- dashboardBody(
#   fluidPage(
#     ENRICHBAR_UI(id = "demo")
#   )
# )
# 
# server <- function(input, output,session) {
#    mydata <- read.delim("www/KEGG_barenrichment.txt", header = T)
#   callModule(ENRICHBAR,"demo", data_frame = mydata)
# }
# shinyApp(ui, server)
# 


ENRICHBAR_UI <-function(id){
  ns <- NS(id)
  install.packages.auto(colourpicker)
  # this module returns a fluidRow section taking up the whole page
  fluidRow(
    box(
      width = 4,
      status = "primary", 
      solidHeader = TRUE,
      fluidRow(
        column(6,
               checkboxInput(
                 ns("others_enrichment_showtopN"),
                 "Only Show topN?",
                 FALSE
               ),
               conditionalPanel(
                 condition = paste0("input['", ns("others_enrichment_showtopN"), "']"),
                 sliderInput(
                   ns("others_enrichmentbar_topN"),
                   "Show topN when too many items",
                   min = 5,
                   max = 50,
                   value =20,
                   step = 1
                 )
               ),
               
               selectInput(
                 ns("others_plot_order_by"), 
                 "Order by:", 
                 c("hits", "p-value"),
                 "p-value"
               ),
               sliderInput(
                 ns("others_enrichmentbar_label_margin"),
                 "Width for the labeling",
                 min = 10,
                 max = 30,
                 value =30,
                 step = 1
               ),
               sliderInput(
                 ns("others_enrichmentbar_labelwrap_size"),
                 "If label too long, length to wrap text",
                 min = 20,
                 max = 100,
                 value =60,
                 step = 1
               )
        ),
        column(6,
               sliderInput(
                 ns("others_enrichmentbar_filter_pvalue"),
                 "Filter by p value",
                 min = 0,
                 max = 1,
                 value =1,
                 step = 0.001
               ),
               textInput(
                 ns("others_plot_maintitle"),
                 "Main title",
                 "Enrichment plot"
               ),
               colourInput(
                 ns("others_plot_bar_color"), 
                 "Color for bar", 
                 "#458B00",
                 #showColour = "background",
                 palette = "limited"
               ),
               colourInput(
                 ns("others_plot_line_color"), 
                 "Color for Line", 
                 "#FF7F00",
                 #showColour = "background",
                 palette = "limited"
               ),
               sliderInput(
                 ns("others_enrichmentbar_labelfont_size"),
                 "Font size scale for labeling text",
                 min = 0.3,
                 max = 2,
                 value =0.6,
                 step = 0.1
               )
        )
      ),
      
      fluidRow(
        column(6),
        column(6,
               actionButton(ns("button_others_plot"), 
                            icon = icon("paper-plane"),
                            label = "Plot",
                            style="float:right; color: #fff; background-color: #337ab7; border-color: #2e6da4"
               )
        )
      )
      
    ),
    uiOutput(ns("ui_enrichbar_plot"))
  )
  
}


ENRICHBAR <- function(input, output, session, data_frame){
  ns <- session$ns
  
  # take over the ouput 
  df<- as.data.frame(data_frame)
  
  observe({
    if(input$button_others_plot > 0 ){
      isolate({
        # do filtering by p value
        df <-df[df[,3] <= input$others_enrichmentbar_filter_pvalue,]
        
        enrichment_plot <- plot_bar_KEGG_2(
          data = df, 
          show_topN = input$others_enrichment_showtopN,
          maxN = as.numeric(input$others_enrichmentbar_topN), 
          title = input$others_plot_maintitle, 
          xlabel_margin =as.numeric(input$others_enrichmentbar_label_margin), 
          order_by = input$others_plot_order_by,
          col_bar = input$others_plot_bar_color,
          col_line = input$others_plot_line_color,
          label_wrap_size = as.numeric(input$others_enrichmentbar_labelwrap_size),
          label_font_size = as.numeric(input$others_enrichmentbar_labelfont_size)
        )
        callModule(plotInBox, "enrichment_plot",
                   plot_object = enrichment_plot,
                   plot_type = "recordPlot",
                   name_tag = "enrichment_plot")
        
        output$ui_enrichbar_plot <- renderUI({
          plotInBox_UI(id = ns("enrichment_plot"), 
                       boxtitle = NULL,
                       boxwidth = 8)
        })
        
        
      })
    }
  })
  
}




# mode for plotting heatmap, including a d3 heatmap and classical hetamp2
# this module could be used as a fast explore of matrix data
# the heatmap2 function uses the usecolor module to have plenty of color options

# ui does not take any extra parameter
# server function only takes one argument of data_matrix to plot
# 

# minimum example:

# library(shiny)
# library(shinydashboard)
# source("shiny_modules.R")
# 
# ui <- dashboardBody(
#   fluidPage(
#     HEATMAP_UI(id = "demo")
#   )
# )
# 
# server <- function(input, output,session) {
#   callModule(HEATMAP,"demo", data_matrix = mtcars)
# }
# shinyApp(ui, server)
# 
HEATMAP2_UI <-function(id, show_color_options = FALSE){
  ns <- NS(id)
  # this module returns a fluidRow section taking up the whole page
  fluidRow(
    box(
      width = 4,
      solidHeader = TRUE,
      status = "primary",
      
        wellPanel(
          fluidRow(
            column(6,
                   selectInput(ns("D3heatmap_scale"), 
                               "Scale data?", 
                               c("none", "row", "column"),
                               "none"),
                   checkboxInput(ns("D3heatmap_na.rm"), "Remove empty values?",value = TRUE),
                   checkboxInput(ns("D3heatmap_Rowv"), "Rows clustering?", value = TRUE),
                   checkboxInput(ns("D3heatmap_Colv"), "Columns clustering?", value = TRUE)
                   ),
            column(6,
                   selectInput(ns("D3heatmap_dendrogram"), "Display dendrogram?",c("none","row","column", "both"), "both"),
                   
                   selectInput(ns("D3heatmap_Dist"), "Method to calculate the distance", 
                               c("euclidean", "maximum", "manhattan", "canberra","binary","minkowski"),"euclidean"),
                   selectInput(ns("D3heatmap_hclustfun"), "Method to cluster", 
                               c("complete", "average", "single", "mcquitty","median","centroid"),"complete")
                   
            )
          )
          
        ),
        wellPanel(
          fluidRow(
            column(6,
                   textInput(ns("heatmap2_title"), "Main title", value = ""),
                   textInput(ns("heatmap2_xlab"), "X title", value = ""),
                   textInput(ns("heatmap2_ylab"), "Y title", value = "")
 
            ),
            column(6,
                   sliderInput(ns("heatmap2_row_margin"), "Row label margin", min = 2, max = 20, value = 5),
                   sliderInput(ns("heatmap2_col_margin"), "column label margin", min = 2, max = 50, value = 20)
                   
                   
            )
          )
          
        #)
        
      ),
      wellPanel(
        checkboxInput(ns("heatmap2_key_show"), "More options for colorkey/scale ?",value = FALSE),
        conditionalPanel(
          condition = paste0("input['", ns("heatmap2_key_show"), "']"),
          
          fluidRow(
            column(6,
                   checkboxInput(ns("heatmap2_key"), "Show color key?",value = TRUE),
                   sliderInput(ns("heatmap2_keysize"), label = "color key size", min = 1, 
                               max = 4, value = 1.5, step = 0.1),
                   selectInput(ns("heatmap2_density.info"), 
                               "Overlap color key with", 
                               c("none","histogram", "density")
                   )
            ),
            column(6, 
                   textInput(ns("heatmap2_key.title"), "Color key title", value = ""),
                   textInput(ns("heatmap2_key.xlab"), "Color key x title", value = ""),
                   textInput(ns("heatmap2_key.ylab"), "Color key y title", value = "")
                   
            )
          )
        ),
        useColor_UI(ns("correlation_matrix_color"),
                    initialize_show = show_color_options,
                    method = "RcolorBrewer",
                    Rcolorbrewer_schemes = "div",
                    RcolorBrewer_theme = "BrBG",
                    ncolors = 100)
      ),
      
      fluidRow(
        column(12,
               actionButton(ns("button_apply_plotheatmap"), 
                            icon = icon("paper-plane"),
                            label = "Plot",
                            style="float:right; color: #fff; background-color: #337ab7; border-color: #2e6da4"
               )
        )
        
      )
      
    ),
  
      uiOutput(ns("ui_heatmap2"))
    
  )
  
}

HEATMAP2_server <- function(input, output, session, data_matrix, sample_group_factor = NULL){
  ns <- session$ns
  install.packages.auto(gplots) # heatmap.2
  # take over the ouput 
  t<- as.matrix(data_matrix)
  
  # get colors first, do not put into isolate
  colors <- callModule(useColor, "correlation_matrix_color")
  
  

    
  observe({
    
    if(input$button_apply_plotheatmap > 0){
      isolate({
          svg(tempfile())
          dev.control('enable') 
          
          if(!is.null(sample_group_factor)){
            sample_group_factor <-  factor2color(sample_group_factor)
            heatmap.2(t,
                      trace = "none",
                      Rowv = input$D3heatmap_Rowv, 
                      Colv = input$D3heatmap_Colv,
                      dendrogram = input$D3heatmap_dendrogram,
                      col = colors(),
                      distfun = function(x, method = input$D3heatmap_Dist){dist(x, method = method)},
                      hclustfun = function(x, method = input$D3heatmap_hclustfun){hclust(x, method=method)},
                      scale = input$D3heatmap_scale,
                      margin = c(input$heatmap2_row_margin,input$heatmap2_col_margin), # controls the margine of the x,y labeling
                      lhei= c(1, 5), # controls the relative height of the color key to mainplot (1:5 ratio of height)
                      key = input$heatmap2_key,
                      keysize = as.numeric(input$heatmap2_keysize),
                      density.info = input$heatmap2_density.info,
                      key.title = input$heatmap2_key.title,
                      key.xlab = input$heatmap2_key.xlab,
                      key.ylab = input$heatmap2_key.ylab,
                      ColSideColors = sample_group_factor,
                      colCol=sample_group_factor
            ) 
          }else{
            heatmap.2(t,
                      trace = "none",
                      Rowv = input$D3heatmap_Rowv, 
                      Colv = input$D3heatmap_Colv,
                      dendrogram = input$D3heatmap_dendrogram,
                      col = colors(),
                      distfun = function(x, method = input$D3heatmap_Dist){dist(x, method = method)},
                      hclustfun = function(x, method = input$D3heatmap_hclustfun){hclust(x, method=method)},
                      scale = input$D3heatmap_scale,
                      margin = c(input$heatmap2_row_margin,input$heatmap2_col_margin), # controls the margine of the x,y labeling
                      lhei= c(1, 5), # controls the relative height of the color key to mainplot (1:5 ratio of height)
                      key = input$heatmap2_key,
                      keysize = as.numeric(input$heatmap2_keysize),
                      density.info = input$heatmap2_density.info,
                      key.title = input$heatmap2_key.title,
                      key.xlab = input$heatmap2_key.xlab,
                      key.ylab = input$heatmap2_key.ylab
            ) 
          }
          
         
          
          heatmap2_plot <- recordPlot()
          dev.off()
          
          
          callModule(plotInBox, "heatmap2_plot",
                     plot_object = heatmap2_plot,
                     plot_type = "recordPlot",
                     name_tag = "heatmap2 plot")
          
          output$ui_heatmap2 <- renderUI({
            plotInBox_UI(ns("heatmap2_plot"), 
                         boxtitle = NULL,
                         boxwidth = 8)
          })
          
        
        
      })
      
    }
    
  })
  
}




HEATMAP_UI <-function(id, show_color_options = FALSE){
  ns <- NS(id)
  # this module returns a fluidRow section taking up the whole page
  fluidRow(
    box(
      width = 4,
      solidHeader = TRUE,
      status = "primary",
      
      fluidRow(
        column(6,
               checkboxInput(ns("simple_data_process"),
                             "Process data before plotting?",
                             FALSE
               )
        )
      ),
      
      conditionalPanel(
        condition = paste0("input['", ns("simple_data_process"), "']"),
        fluidRow(
          column(6,
                 selectInput(ns("Log_transformation"),
                             "Log transformation",
                             c("no transform","log2", "log10", "-log10")
                 )
          ),
          column(6,
                 checkboxInput(ns("transpose_matrix"),
                               "Transpose matrix?",
                               FALSE
                 )
          )
        ),
        fluidRow(
          column(6,
                 checkboxInput(ns("Filter_out_values"),
                               "Filter out values",
                               FALSE
                 )
          )
        ),
        
        conditionalPanel(
          condition = paste0("input['", ns("Filter_out_values"), "']"),
          fluidRow(
            column(6,
                   
                   selectInput(ns("filter_condition"),
                               "Filter condtion",
                               c("<=", "<", ">=", ">")
                   ),
                   textInput(ns("Filter_value"),
                             "Filter by value",
                             "0"
                   )
                   
            ),
            column(6,
                   textInput(ns("valid_value_number_row"),
                             "Number of valid values for each row",
                             "1"
                   ),
                   textInput(ns("valid_value_number_col"),
                             "Number of valid values for each column",
                             "1"
                   )
            )
          )
        )
        
      ),
      
      fluidRow(
        column(6,
               selectInput(ns("heatmap_plot_method"), "Method to plot heamap", 
                           c("Interactive D3 heatmap" = "d3heatmp", "Static heatmp" = "heatmap2"),
                           "heatmap2"),
               
               selectInput(ns("D3heatmap_scale"), "Scale data?", 
                           c("none", "row", "column"),"column"),
               checkboxInput(ns("D3heatmap_na.rm"), "Remove empty values?",value = TRUE),
               checkboxInput(ns("D3heatmap_Rowv"), "Rows clustering?", value = TRUE),
               checkboxInput(ns("D3heatmap_Colv"), "Columns clustering?", value = TRUE),
               
               selectInput(ns("D3heatmap_dendrogram"), "Display dendrogram?",c("none","row","column", "both"), "both"),
               
               selectInput(ns("D3heatmap_Dist"), "Method to calculate the distance", 
                           c("euclidean", "maximum", "manhattan", "canberra","binary","minkowski"),"euclidean"),
               selectInput(ns("D3heatmap_hclustfun"), "Method to cluster", 
                           c("complete", "average", "single", "mcquitty","median","centroid"),"complete")
               
        ),
        column(6,
               
               conditionalPanel(
                 condition = paste0("input['", ns("heatmap_plot_method"), "']", "=='d3heatmp'"),
                 selectInput(ns("D3heatmap_color"), 
                             "Color theme", 
                             c("YlOrRd","YlGnBu", "YlGn","RdYlBu", 
                               "Greens", "Blues", "Reds", "Greys")
                 ),
                 textInput(ns("D3heatmap_plot_height"), "Plot height(px)", value = "800"),
                 sliderInput(ns("D3heatmap_plot_width"), label = "Plot width %", min = 1, 
                             max = 100, value = 100)
               ),
               
               
               
               conditionalPanel(
                 condition = paste0("input['", ns("heatmap_plot_method"), "']", "=='heatmap2'"),
                 textInput(ns("heatmap2_title"), "Main title", value = ""),
                 textInput(ns("heatmap2_xlab"), "X title", value = ""),
                 textInput(ns("heatmap2_ylab"), "Y title", value = ""),
                 sliderInput(ns("heatmap2_row_margin"), "Row label margin", min = 2, max = 20, value = 5),
                 sliderInput(ns("heatmap2_col_margin"), "column label margin", min = 2, max = 50, value = 20),
                 
                 checkboxInput(ns("heatmap2_key_show"), "More options for colorkey/scale ?",value = FALSE)
               )
        )
      ),
      
      conditionalPanel(
        condition = paste0("input['", ns("heatmap2_key_show"), "']"),
        
        fluidRow(
          column(6,
                 checkboxInput(ns("heatmap2_key"), "Show color key?",value = TRUE),
                 sliderInput(ns("heatmap2_keysize"), label = "color key size", min = 1, 
                             max = 4, value = 1.5, step = 0.1),
                 selectInput(ns("heatmap2_density.info"), 
                             "Overlap color key with", 
                             c("none","histogram", "density")
                 )
          ),
          column(6, 
                 textInput(ns("heatmap2_key.title"), "Color key title", value = ""),
                 textInput(ns("heatmap2_key.xlab"), "Color key x title", value = ""),
                 textInput(ns("heatmap2_key.ylab"), "Color key y title", value = "")
                 
          )
        )
      ),
      
      
      conditionalPanel(
        condition = paste0("input['", ns("heatmap_plot_method"), "']", "=='heatmap2'"),
        fluidRow(
          column(12,
                 useColor_UI(ns("correlation_matrix_color"),
                             initialize_show = show_color_options,
                             method = "RcolorBrewer",
                             Rcolorbrewer_schemes = "div",
                             RcolorBrewer_theme = "BrBG",
                             ncolors = 100)
          )
          
          
        )
      ),
      
      fluidRow(
        column(6),
        column(6,
               actionButton(ns("button_apply_plotheatmap"), 
                            icon = icon("paper-plane"),
                            label = "Plot",
                            style="float:right; color: #fff; background-color: #337ab7; border-color: #2e6da4"
               )
        )
        
      )
      
    ),
    
    #ui Output
    conditionalPanel(
      condition = paste0("input['", ns("heatmap_plot_method"), "']", "=='d3heatmp'"),
      uiOutput(ns("ui_d3heatmap"))
    ),
    conditionalPanel(
      condition = paste0("input['", ns("heatmap_plot_method"), "']", "=='heatmap2'"),
      uiOutput(ns("ui_heatmap2"))
    )
  )
  
}

HEATMAP <- function(input, output, session, data_matrix, sample_group_factor = NULL){
  ns <- session$ns
  install.packages.auto(gplots) # heatmap.2
  # take over the ouput 
  t<- as.matrix(data_matrix)
  
  # get colors first, do not put into isolate
  colors <- callModule(useColor, "correlation_matrix_color")
  
  
  observe({
    
    if(input$button_apply_plotheatmap > 0){
      isolate({
        t <- switch(input$Log_transformation,
                    "no transform" = t,
                    "log2" = {log2(t)},
                    "log10" = {log10(t)},
                    "-log10" = {-log10(t)}
                    
        )
        if(input$transpose_matrix){
          t <-t(t)
        }
        
        #
        
        if(input$Filter_out_values){
          t <- switch(input$filter_condition,
                      "<=" = {
                        t[t <= as.numeric(input$Filter_value)] <- 0
                        t[which((ncol(t) -rowSums(t == 0)) >= as.numeric(input$valid_value_number_row)),  
                          which((nrow(t) -colSums(t == 0)) >= as.numeric(input$valid_value_number_col))]
                      },
                      "<" = {
                        t[t < as.numeric(input$Filter_value)] <- 0
                        t[which((ncol(t) -rowSums(t == 0)) >= as.numeric(input$valid_value_number_row)),  
                          which((nrow(t) -colSums(t == 0)) >= as.numeric(input$valid_value_number_col))]
                      },
                      
                      ">=" = {
                        t[t >= as.numeric(input$Filter_value)] <- 0
                        t[which((ncol(t) -rowSums(t == 0)) >= as.numeric(input$valid_value_number_row)),  
                          which((nrow(t) -colSums(t == 0)) >= as.numeric(input$valid_value_number_col))]
                        
                      },
                      ">" = {
                        t[t > as.numeric(input$Filter_value)] <- 0
                        t[which((ncol(t) -rowSums(t == 0)) >= as.numeric(input$valid_value_number_row)),  
                          which((nrow(t) -colSums(t == 0)) >= as.numeric(input$valid_value_number_col))]
                        
                      }
                      
          )
        }
        
        
        if(input$heatmap_plot_method == "d3heatmp"){
          output$heatmapd3 <- renderD3heatmap({
            d3heatmap(
              t,
              colors = input$D3heatmap_color,
              Rowv = input$D3heatmap_Rowv, 
              Colv = input$D3heatmap_Colv,
              dendrogram = input$D3heatmap_dendrogram,
              k_row = input$D3heatmap_k_row,
              k_col = input$D3heatmap_k_col,
              distfun = function(x, method = input$D3heatmap_Dist){dist(x, method = method)},
              hclustfun = function(x, method = input$D3heatmap_hclustfun){hclust(x, method=method)},
              scale = input$D3heatmap_scale,
              main = input$heatmap2_title,
              xlab = input$heatmap2_xlab,
              ylab = input$heatmap2_ylab
            )
          })
          
          # generate ui for d3 heatmap
          output$ui_d3heatmap <- renderUI({
            box(
              width = 8,
              solidHeader = TRUE,
              status = "primary",
              
              d3heatmapOutput(ns("heatmapd3"),
                              width = paste0(input$D3heatmap_plot_width, "%"),
                              height = paste0(input$D3heatmap_plot_height, "px"))
            )
            
          })
          
        }
        
        # for heatmap2
        if(input$heatmap_plot_method == "heatmap2"){
          
          svg(tempfile())
          dev.control('enable') 
          heatmap.2(t,
                    trace = "none",
                    Rowv = input$D3heatmap_Rowv, 
                    Colv = input$D3heatmap_Colv,
                    dendrogram = input$D3heatmap_dendrogram,
                    col = colors(),
                    distfun = function(x, method = input$D3heatmap_Dist){dist(x, method = method)},
                    hclustfun = function(x, method = input$D3heatmap_hclustfun){hclust(x, method=method)},
                    scale = input$D3heatmap_scale,
                    margin = c(input$heatmap2_row_margin,input$heatmap2_col_margin), # controls the margine of the x,y labeling
                    lhei= c(1, 5), # controls the relative height of the color key to mainplot (1:5 ratio of height)
                    key = input$heatmap2_key,
                    keysize = as.numeric(input$heatmap2_keysize),
                    density.info = input$heatmap2_density.info,
                    key.title = input$heatmap2_key.title,
                    key.xlab = input$heatmap2_key.xlab,
                    key.ylab = input$heatmap2_key.ylab
          ) # plot
          
          heatmap2_plot <- recordPlot()
          dev.off()
          
          
          callModule(plotInBox, "heatmap2_plot",
                     plot_object = heatmap2_plot,
                     plot_type = "recordPlot",
                     name_tag = "heatmap2 plot")
          
          output$ui_heatmap2 <- renderUI({
            plotInBox_UI(ns("heatmap2_plot"), 
                         boxtitle = NULL,
                         boxwidth = 8)
          })
          
        }
        
        
      })
      
    }
    
  })
  
}




ColorGenerator_UI <- function(id, 
                              caption = "Set Colors",
                              method = "RcolorBrewer", # choose the method for color generation
                              Rcolorbrewer_schemes = "div", # this only works when Rcolorbrewer is selected
                              RcolorBrewer_theme = "Spectral", # this only works when the right Rcolorbrewer and schemes are selected 
                              RPalettes = NULL, # choose the r built in color
                              Classical_color = NULL,
                              DIY_color_mode = NULL
                              
){
  ns <- NS(id)
  install.packages.auto(shinyBS)
  #sequential, diverging, or qualitative schemes
  tagList(
    actionButton(paste0(id,"button_pop"),  #bsModal has a bug that it does not support namespace, use past0 instead
                 strong(caption)),
    bsModal(ns("popupwindow_color"), 
            "Advanced Color Generator", 
            paste0(id,"button_pop"),  #bsModal has a bug that it does not support namespace, use past0 instead
            size = "large",
            fluidRow(
              column(6,
                     selectInput(ns("Coloring_method"),
                                 "Choose coloring method",
                                 c("RcolorBrewer", "R built-in Palettes","Classical color collection", "DIY colors"),
                                 selected = method),
                     # conditional display when RcolorBrewer chosen
                     conditionalPanel(
                       condition = paste0("input['", ns("Coloring_method"), "']", "=='RcolorBrewer'"),
                       selectInput(ns("Rcolorbrewer_schemes"),
                                   "Choose color type",
                                   c("RcolorBrewer: sequential" = "seq",
                                     "RcolorBrewer: Quality" = "qual",
                                     "RcolorBrewer: Diverging" = "div"),
                                   selected = Rcolorbrewer_schemes
                       ),
                       uiOutput(ns("ui_RcolorBrewer_themes_choose"))
                       
                     ),
                     # textinput to pass a parameter to the server end, always hide
                     conditionalPanel(
                       condition = "1 > 2",# here it needs a javascript expression to be FALSE
                       textInput(ns("RcolorBrewer_themes_selected"),
                                 "hidden textinput used for passing argument",
                                 value =  RcolorBrewer_theme)
                     ),
                     # conditional display when R built-in Palettes chosen
                     conditionalPanel(
                       condition = paste0("input['", ns("Coloring_method"), "']", "=='R built-in Palettes'"),
                       selectInput(ns("Rbuiltin_Palettes"),
                                   "Choose color type",
                                   c("rainbow","heat.colors","terrain.colors", "topo.colors","cm.colors"),
                                   selected = RPalettes
                       )
                     ),
                     # conditional display when Classical color collection chosen
                     conditionalPanel(
                       condition = paste0("input['", ns("Coloring_method"), "']", "=='Classical color collection'"),
                       selectInput(ns("Classical_color_collection"),
                                   "Choose color type",
                                   c("red-back-green","blue-white-red"),
                                   selected = Classical_color
                       )
                     ),
                     # conditional display when DIY color chosen
                     conditionalPanel(
                       condition = paste0("input['", ns("Coloring_method"), "']", "=='DIY colors'"),
                       selectInput(ns("DIY_color_mode"),
                                   "DIY color mode",
                                   c("single color","Dual-Color gradient","Tri-Color gradient"),
                                   selected = DIY_color_mode
                       ),
                       uiOutput(ns("ui_DIY_color_picker"))
                     )
              ),
              column(6,
                     uiOutput(ns("ui_Number_of_colors_to_generate")),
                     # numericInput(ns("Number_of_colors_to_generate"),
                     #              "How many colors do you want to generate?",
                     #              min= 3,
                     #              step = 1,
                     #              value = ncolors
                     # ),
                     checkboxInput(ns("If_rev_colors"),
                                   "Reverse colors?",
                                   FALSE)
              )
            )
            ,
            fluidRow(
              column(12,
                     strong("Preview generated colors:"),
                     plotOutput(ns("plot_selected_color_theme"), height = "200px")
              )
            )
            
    )
    
  )
  
  
}

# this server function will return the colors generated
ColorGenerator_server <- function(input, output, session, 
                                  Rcolorbrewer_schemes =  "div", # this is for the default color output
                                  RcolorBrewer_theme = "Spectral", # this is for the default color output
                                  ncolors = 10
){
  
  ns <- session$ns
  
  install.packages.auto(colourpicker)
  install.packages.auto("RColorBrewer")
  install.packages.auto(shinyBS)
  
  # ui rendering part
  # for how many color to generate

  output$ui_Number_of_colors_to_generate <- renderUI({
    numericInput(ns("Number_of_colors_to_generate"),
                 "How many colors do you want to generate?",
                 min= 1,
                 step = 1,
                 value = ncolors
    )
  })
  
  # dynamic UI generateion for Rcolor brewer and DIY cololrs
  observe({
    switch(input$Coloring_method,
           "RcolorBrewer" = {
             # get the theme list of each scheme
             RcolorBrewer_theme_list <-  switch(input$Rcolorbrewer_schemes,
                                                "seq" = {rownames(brewer.pal.info[brewer.pal.info$category == "seq",])},
                                                "qual" = {rownames(brewer.pal.info[brewer.pal.info$category == "qual",])},
                                                "div" = {rownames(brewer.pal.info[brewer.pal.info$category == "div",])}
             )
             # generate a ui for the theme selection
             output$ui_RcolorBrewer_themes_choose <- renderUI({
               selectInput(ns("RcolorBrewer_theme"),
                           "Choose RcolorBrewer color themes",
                           RcolorBrewer_theme_list,
                           selected = input$RcolorBrewer_themes_selected
               )
             })
           },
           
           "DIY colors" = {
             switch(input$DIY_color_mode,
                    "single color" = {
                      output$ui_DIY_color_picker <- renderUI({
                        colourInput(
                          ns("color_picker_single"), 
                          "Choose color", 
                          "#7F7F7F"
                          #showColour = "background",
                          #palette = "limited"
                        )
                      })
                    },
                    "Dual-Color gradient" = {
                      output$ui_DIY_color_picker <- renderUI({
                        tagList(
                          colourInput(
                            ns("color_picker_low"), 
                            "Low color", 
                            "#FA0000"
                          ),
                          colourInput(
                            ns("color_picker_high"), 
                            "High color", 
                            "#00B3FA"
                          )
                        )
                        
                      })
                    },
                    
                    "Tri-Color gradient" = {
                      output$ui_DIY_color_picker <- renderUI({
                        tagList(
                          colourInput(
                            ns("color_picker_low"), 
                            "Low color", 
                            "#FA0000"
                            #showColour = "background",
                            #palette = "limited"
                          ),
                          colourInput(
                            ns("color_picker_middle"), 
                            "Middle color", 
                            "#7F7F7F"
                          ),
                          colourInput(
                            ns("color_picker_high"), 
                            "High color", 
                            "#00B3FA"
                          )
                        )
                        
                      })
                      
                    }
                    
             )
           }
           
    ) 
    
  })
  
  
  # get the colors from the ui settings
  mycol_final <- reactive({
    
    if(!is.null(input$Number_of_colors_to_generate)){
      
      mycol <- switch(input$Coloring_method,
                      "RcolorBrewer" = {
                        if(!is.null(input$RcolorBrewer_theme)){
                          
                          color_theme_name <- input$RcolorBrewer_theme
                          #print(color_theme_name)
                          max_color <- brewer.pal.info[which(rownames(brewer.pal.info) == color_theme_name),1]
                          
                          # get the colorbrewer_colors
                          
                          my_colbrew_colors <- brewer.pal(max_color,color_theme_name)
                          #print(my_colbrew_colors)
                          # return the color generated
                          colorRampPalette(my_colbrew_colors)(input$Number_of_colors_to_generate)
                          
                        }else{
                          #avoid returnning null to cause error 
                          rainbow(input$Number_of_colors_to_generate)
                        }
                        
                      },
                      "R built-in Palettes" = {
                        switch(input$Rbuiltin_Palettes,
                               "rainbow" = {rainbow(input$Number_of_colors_to_generate)},
                               "heat.colors" = {heat.colors(input$Number_of_colors_to_generate)},
                               "terrain.colors"  = {terrain.colors(input$Number_of_colors_to_generate)},
                               "topo.colors"  = {topo.colors(input$Number_of_colors_to_generate)},
                               "cm.colors" = {cm.colors(input$Number_of_colors_to_generate)}
                        )
                      },
                      "Classical color collection" = {
                        switch (input$Classical_color_collection,
                                "red-back-green" = {colorRampPalette(c("red", "black", "green"))(input$Number_of_colors_to_generate)},
                                "blue-white-red" = {colorRampPalette(c("blue", "white", "red"))(input$Number_of_colors_to_generate)}
                        )
                        
                      },
                      "DIY colors" = {
                        # take the input of the newly generated ui and then generate the color gradient
                        switch (input$DIY_color_mode,
                                "single color" = {
                                  #print(input$color_picker_single)
                                  input$color_picker_single},
                                "Dual-Color gradient" = {
                                  if(!is.null(input$color_picker_high)){
                                    colorRampPalette(c(input$color_picker_low, input$color_picker_high))(input$Number_of_colors_to_generate)
                                  }
                                },
                                
                                "Tri-Color gradient" = {
                                  if(!is.null(input$color_picker_high)){
                                    colorRampPalette(c(input$color_picker_low, input$color_picker_middle,input$color_picker_high))(input$Number_of_colors_to_generate)
                                  }
                                }
                        )
                        
                      }
      )
      #)
      
      
    }else{
      # this is the temporary and default color if the color generator is not active
      # this is mandatory, because the reactive value needs to be not null to return
      mycol =  ColorGenerator(ncolors = ncolors,
                              Rcolorbrewer_schemes = Rcolorbrewer_schemes,
                              RcolorBrewer_theme = RcolorBrewer_theme
      )
    }
    
    
    ## reverse color if user choose
    if(input$If_rev_colors){
      mycol <- rev(mycol)
    }
    
    return(mycol)
    
  })
  
  
  # output the preview color plot only when my_col_final() is not null
  observe({
    
    if(!is.null(mycol_final()) && !is.null(input$Number_of_colors_to_generate)){
      
      output$plot_selected_color_theme <- renderPlot({
        par(mar=c(0.1, 0.1, 0.1, 0.1)) 
        barplot(rep(1,input$Number_of_colors_to_generate),
                col = mycol_final(),
                yaxt =  "n",
                border = NA
        )
        graphics::box("outer",col="black") # put a box around the bar
      })
      
      #})
      
    }
  })
  
  # 
  
  
  # return colors
  
  return(mycol_final)
  
}








# colorpicker module, is always something I wanted to do, to make life easier
# usage:
# see shiny_modue_colorpickingup_demo.R for demo

# note!!!!!
# refer to module of HEATMAP for the reference
# on the server side, do not put the call module in any isolate enrivoment (but can be in oberve), otherwise,it will not work properly
# always use the retured colors with () in reactive enviroment, because the returned colors are actually reactive expressions

# the returned piece of UI is in a taglList, could be inserted in a box or column

# all parameters can be set in the ui end to customerize the initial colors
# the front arguments will void the later one which are dependent on 

useColor_UI <- function(id = "test", 
                        initialize_show = TRUE,
                        caption = "Show more color selection options",
                        method = NULL, # choose the method for color generation
                        Rcolorbrewer_schemes =  NULL, # this only works when Rcolorbrewer is selected
                        RcolorBrewer_theme = NULL, # this only works when the right Rcolorbrewer and schemes are selected 
                        RPalettes = NULL, # choose the r built in color
                        Classical_color = NULL,
                        DIY_color_mode = NULL,
                        ncolors = 100
){
  ns <- NS(id)
  
  #sequential, diverging, or qualitative schemes
  tagList(
    checkboxInput(ns("Show_more_options_for_color_picker"),
                  caption,
                  initialize_show
    ),
    conditionalPanel(
      condition = paste0("input['", ns("Show_more_options_for_color_picker"), "']"),
      fluidRow(
        column(6,
               selectInput(ns("Coloring_method"),
                           "Choose coloring method",
                           c("RcolorBrewer", "R built-in Palettes","Classical color collection", "DIY colors"),
                           selected = method),
               # conditional display when RcolorBrewer chosen
               conditionalPanel(
                 condition = paste0("input['", ns("Coloring_method"), "']", "=='RcolorBrewer'"),
                 selectInput(ns("Rcolorbrewer_schemes"),
                             "Choose color type",
                             c("RcolorBrewer: sequential" = "seq",
                               "RcolorBrewer: Quality" = "qual",
                               "RcolorBrewer: Diverging" = "div"),
                             selected = Rcolorbrewer_schemes
                 ),
                 uiOutput(ns("ui_RcolorBrewer_themes_choose"))
                 
               ),
               # textinput to pass a parameter to the server end, always hide
               conditionalPanel(
                 condition = "1 > 2",# here it needs a javascript expression to be FALSE
                 textInput(ns("RcolorBrewer_themes_selected"),
                           "hidden textinput used for passing argument",
                           value =  RcolorBrewer_theme)
               ),
               # conditional display when R built-in Palettes chosen
               conditionalPanel(
                 condition = paste0("input['", ns("Coloring_method"), "']", "=='R built-in Palettes'"),
                 selectInput(ns("Rbuiltin_Palettes"),
                             "Choose color type",
                             c("rainbow","heat.colors","terrain.colors", "topo.colors","cm.colors"),
                             selected = RPalettes
                 )
               ),
               # conditional display when Classical color collection chosen
               conditionalPanel(
                 condition = paste0("input['", ns("Coloring_method"), "']", "=='Classical color collection'"),
                 selectInput(ns("Classical_color_collection"),
                             "Choose color type",
                             c("red-back-green","blue-white-red"),
                             selected = Classical_color
                 )
               ),
               # conditional display when DIY color chosen
               conditionalPanel(
                 condition = paste0("input['", ns("Coloring_method"), "']", "=='DIY colors'"),
                 selectInput(ns("DIY_color_mode"),
                             "DIY color mode",
                             c("single color","Dual-Color gradient","Tri-Color gradient"),
                             selected = DIY_color_mode
                 ),
                 uiOutput(ns("ui_DIY_color_picker"))
               )
        ),
        column(6,
               numericInput(ns("Number_of_colors_to_generate"),
                            "How many colors do you want to generate?",
                            min= 3,
                            step = 1,
                            value = ncolors
               ),
               checkboxInput(ns("If_rev_colors"),
                             "Reverse colors?",
                             FALSE),
               checkboxInput(ns("If_preview_colors"),
                             "Preview colors?",
                             FALSE),
               conditionalPanel(
                 condition = paste0("input['", ns("If_preview_colors"), "']"),
                 plotOutput(ns("plot_selected_color_theme"))
               )
        )
      )
    )
  )
  
  
}

# this server function will return the colors generated
useColor <- function(input, output, session){
  
  ns <- session$ns
  
  install.packages.auto(colourpicker)
  install.packages.auto("RColorBrewer")
  
  #rvalues <-reactiveValues()
  # ui rendering part
  observe({
    
    switch(input$Coloring_method,
           "RcolorBrewer" = {
             # get the theme list of each class
             RcolorBrewer_theme_list <-  switch(input$Rcolorbrewer_schemes,
                                                "seq" = {rownames(brewer.pal.info[brewer.pal.info$category == "seq",])},
                                                "qual" = {rownames(brewer.pal.info[brewer.pal.info$category == "qual",])},
                                                "div" = {rownames(brewer.pal.info[brewer.pal.info$category == "div",])}
             )
             # generate a new ui for the theme selection
             output$ui_RcolorBrewer_themes_choose <- renderUI({
               selectInput(ns("RcolorBrewer_theme"),
                           "Choose RcolorBrewer color themes",
                           RcolorBrewer_theme_list,
                           selected = input$RcolorBrewer_themes_selected
               )
             })
           },
           "DIY colors" = {
             switch(input$DIY_color_mode,
                    "single color" = {
                      output$ui_DIY_color_picker <- renderUI({
                        colourInput(
                          ns("color_picker_single"), 
                          "Choose color", 
                          "#7F7F7F"
                          #showColour = "background",
                          #palette = "limited"
                        )
                      })
                    },
                    "Dual-Color gradient" = {
                      output$ui_DIY_color_picker <- renderUI({
                        tagList(
                          colourInput(
                            ns("color_picker_low"), 
                            "Low color", 
                            "#FA0000"
                            #showColour = "background",
                            #palette = "limited"
                          ),
                          colourInput(
                            ns("color_picker_high"), 
                            "High color", 
                            "#00B3FA"
                            #showColour = "background",
                            #palette = "limited"
                          )
                        )
                        
                      })
                    },
                    
                    "Tri-Color gradient" = {
                      
                      output$ui_DIY_color_picker <- renderUI({
                        tagList(
                          colourInput(
                            ns("color_picker_low"), 
                            "Low color", 
                            "#FA0000"
                            #showColour = "background",
                            #palette = "limited"
                          ),
                          colourInput(
                            ns("color_picker_middle"), 
                            "Middle color", 
                            "#7F7F7F"
                            #showColour = "background",
                            #palette = "limited"
                          ),
                          colourInput(
                            ns("color_picker_high"), 
                            "High color", 
                            "#00B3FA"
                            #showColour = "background",
                            #palette = "limited"
                          )
                        )
                        
                      })
                      
                    }
                    
             )
           }
           
    ) 
    
  })
  
  # get the colors from the ui settings
  mycol_final <- reactive({
    mycol <- switch(input$Coloring_method,
                    "RcolorBrewer" = {
                      if(!is.null(input$RcolorBrewer_theme)){
                        
                        color_theme_name <- input$RcolorBrewer_theme
                        #print(color_theme_name)
                        max_color <- brewer.pal.info[which(rownames(brewer.pal.info) == color_theme_name),1]
                        
                        # get the colorbrewer_colors
                        
                        my_colbrew_colors <- brewer.pal(max_color,color_theme_name)
                        #print(my_colbrew_colors)
                        # return the color generated
                        colorRampPalette(my_colbrew_colors)(input$Number_of_colors_to_generate)
                        
                      }else{
                        #avoid returnning null to cause error 
                        rainbow(input$Number_of_colors_to_generate)
                      }
                      
                    },
                    "R built-in Palettes" = {
                      switch(input$Rbuiltin_Palettes,
                             "rainbow" = {rainbow(input$Number_of_colors_to_generate)},
                             "heat.colors" = {heat.colors(input$Number_of_colors_to_generate)},
                             "terrain.colors"  = {terrain.colors(input$Number_of_colors_to_generate)},
                             "topo.colors"  = {topo.colors(input$Number_of_colors_to_generate)},
                             "cm.colors" = {cm.colors(input$Number_of_colors_to_generate)}
                      )
                    },
                    "Classical color collection" = {
                      switch (input$Classical_color_collection,
                              "red-back-green" = {colorRampPalette(c("red", "black", "green"))(input$Number_of_colors_to_generate)},
                              "blue-white-red" = {colorRampPalette(c("blue", "white", "red"))(input$Number_of_colors_to_generate)}
                      )
                      
                    },
                    "DIY colors" = {
                      # take the input of the newly generated ui and then generate the color gradient
                      switch (input$DIY_color_mode,
                              "single color" = {
                                #print(input$color_picker_single)
                                input$color_picker_single},
                              "Dual-Color gradient" = {
                                if(!is.null(input$color_picker_high)){
                                  colorRampPalette(c(input$color_picker_low, input$color_picker_high))(input$Number_of_colors_to_generate)
                                }
                              },
                              
                              "Tri-Color gradient" = {
                                if(!is.null(input$color_picker_high)){
                                  colorRampPalette(c(input$color_picker_low, input$color_picker_middle,input$color_picker_high))(input$Number_of_colors_to_generate)
                                }
                              }
                      )
                      
                    }
    )
    
    ##
    if(input$If_rev_colors){
      mycol <- rev(mycol)
    }
    return(mycol)
    
  })
  
  
  
  # output the plot if 
  observe({
    if(!is.null(mycol_final()) && input$If_preview_colors){
      
      
      
      # if previous switched on
      
      # plot the image
      postscript(tempfile())
      dev.control('enable')
      
      pie(rep(1,input$Number_of_colors_to_generate),
          col = mycol_final(),
          labels = NA,
          radius = 1,
          border = NA
      )
      
      color_display <- recordPlot()
      dev.off()
      
      # generate the UI
      output$plot_selected_color_theme <- renderPlot({
        replayPlot(color_display)
      })
    }
  })
  
  # 
  
  
  # return colors
  
  return(mycol_final)
  
}




datatableProcess_full_UI <- function(id) {
  
  install.packages.auto(dplyr)
  
  ns <- NS(id)
  fluidRow(
    column(4, 
     box(
       width = 12,
       solidHeader = TRUE,
       status = "primary",
       wellPanel(
         h4("NOTE"),
         "Note that this pre-process is optional, and please do it before downstream analysis if necessary, instead of after"
       )
       
       ),
      box(
        width = 12,
        solidHeader = TRUE,
        status = "primary",
       
       # ui for manual selection   
       
       checkboxInput(ns('manual_select_row_column'), 'Manual selection of rows and columns', FALSE),
       
       conditionalPanel(
         condition = paste0("input['", ns("manual_select_row_column"), "']"),
         wellPanel(
           # width =12,
           # status = "primary",
           # solidHeader = TRUE,
           fluidRow(
             column(6, uiOutput(ns("ui_selectInput_select_row"))),
             column(6, uiOutput(ns("ui_selectInput_select_column")))
           )
           
         )
         
       ),
       
       # ui for selection by pattern for rows  
       
       checkboxInput(ns('RegExp_select_row'), 'Batch selection of rows by Name-Pattern', FALSE),
       
       conditionalPanel(
         condition = paste0("input['", ns("RegExp_select_row"), "']"),
         wellPanel(
           selectInput(ns("regexp_type_row"), "Pattern type:",
                       choices = list("starts with", "ends with","contains"),
                       selected = "starts with"),
           fluidRow(
             column(6, 
                    textInput(ns("regexp_value_row"), "Type the text to match", value = "e.g. sample_")
               ),
             column(6,
                    uiOutput(ns("ui_selectInput_available_rownames"))
                    )
             )
         )
       ),
       
       # ui for selection by pattern for columns  
       
       checkboxInput(ns('RegExp_select_column'), 'Batch selection of columns by Name-Pattern', FALSE),
       
       conditionalPanel(
         condition = paste0("input['", ns("RegExp_select_column"), "']"),
         wellPanel(
           selectInput(ns("regexp_type_column"), "Pattern type:",
                       choices = list("starts with", "ends with","contains"),
                       selected = "starts with"),
           fluidRow(
             column(6, 
                    textInput(ns("regexp_value_column"), "Type the text to match", value = "e.g. variable_")
             ),
             column(6,
                    uiOutput(ns("ui_selectInput_available_colnames"))
             )
             
           )
           
         )
       ),
       
       # ui for filtering out missing values 
       checkboxInput(ns('missingvalue_filtering'), 'Filtering out rows with missing values', FALSE),
       
       conditionalPanel(
         condition = paste0("input['", ns("missingvalue_filtering"), "']"),
         wellPanel(
           selectizeInput(ns("matrix_filter_NA_type"),
                          "Type of NA value in data",
                          choices = c("NA", "0", "Inf","-Inf" ),
                          selected = c("NA", "0", "Inf","-Inf"),
                          multiple = TRUE,
                          options = list(plugins=list('drag_drop','remove_button'))
           ),
           sliderInput(ns("missingvalue_filtering_q"), label = "Q value of presence", min = 0,
                       max = 1, value = 0.75),
           helpText("\tQ vaule is the presence percentage threshold. For example, 0,75 means 75% of presence across all samples")
           # bsTooltip(ns("missingvalue_filtering_q"), "Q vaule is the presence percentage threshold",
           #           "right", options = list(container = "body"))
         )
       ),
       

       
       # ui for missing value imputation  
       
       checkboxInput(ns('matrix_inputation'), 'Sequential missing value impuation', FALSE),
       conditionalPanel(
         condition = paste0("input['", ns("matrix_inputation"), "']"),
         wellPanel(
           selectizeInput(ns("matrix_inputation_NA_type"),
                       "Type of NA value in data",
                       choices = c("NA", "0", "Inf","-Inf" ),
                       selected = c("NA", "0", "Inf","-Inf"),
                       multiple = TRUE,
                       options = list(plugins=list('drag_drop','remove_button'))
                       ),
           selectInput(ns("imputation_by"), 
                       "Imnputation by:",
                       choices = c("row","column" ), 
                       selected = "row"),
           sliderInput(ns("matrix_inputation_alpha"), label = "Alpha/Strength:", min = 0,
                       max = 1, value = 0.9)
         )
         
       ),
       
       # ui for log transformation  
       checkboxInput(ns('matrix_transform_switch'), 'Log transform the whole data matrix', FALSE),
       
       conditionalPanel(
         condition = paste0("input['", ns("matrix_transform_switch"), "']"),
         wellPanel(
           selectInput(ns("log_transform"), "Log tranform?",
                       choices = list("log10 tranform", "log2 tranform","ln tranform"),
                       selected = "log10 tranform")
           
         )
         
       ),
       # # ui for scaling  
       
       checkboxInput(ns('scale_table'), 'Normalize/Standarize/Scale/Z-score table', FALSE),
       conditionalPanel(
         condition = paste0("input['", ns("scale_table"), "']"),
         wellPanel(
           selectInput(ns("scale_by"),
                       "apply on:",
                       choices = c("row","column", "row then column", "column then row"),
                       selected = "column"),
           checkboxInput(ns("scale_center"),
                         "Force same means by subtracting means (omitting NAs)",
                         TRUE
                         ),
           checkboxInput(ns("scale_scale"),
                         "Force same SDs by dividing by SD/root mean square)",
                         TRUE
           ),
           helpText("\tThis function centers and scales numeric matrix. ",
                    "Center means the data(row or column)'s mean is going to be 0. ",
                    "Scale is done after centering. Scale is done by divding each value by their standard deviation  if center is TRUE, and the root mean square otherwise.",
                    "This function is called z-score some elsewhere. "
           )
           
           
         )
       ),
       # ui for reverse table 
       checkboxInput(ns('Reverse_table'), 'Reverse table', FALSE),
       conditionalPanel(
         condition = paste0("input['", ns("Reverse_table"), "']"),
         wellPanel(
           checkboxInput(ns("reverse_minus_value"),
                         "Convert all to minus value (-x)",
                         FALSE
           ),
           checkboxInput(ns("reverse_reciprocal_value"),
                         "Convert all to reciprocal value(1/x)",
                         FALSE
           )
         )
        
         ),
       # ui for transpose  
       checkboxInput(ns('transpose_table'), 'Transpose table', FALSE),
       
       
       
       # apply button
       actionButton(ns("button_apply_processing"), 
                    icon = icon("paper-plane"),
                    label = "apply Selection(s)",
                    style="float:left; color: #fff; background-color: #337ab7; border-color: #2e6da4"
       )
       #,
       
       
      )     
    ),
      
      #ui for result summary View 
      column(8,
               tabBox(
                 width = 12,
                 tabPanel("Summary",
                          #display summary
                          fluidRow(
                            uiOutput(ns("ui_process_summary")),
                            # display check scale plot if data is scaled
                            uiOutput(ns("ui_boxplot_check_scale")),
                            uiOutput(ns("ui_table_filtered_out")),
                            uiOutput(ns("ui_plot_profile_comparison")),
                            uiOutput(ns("ui_processedata_download"))
                          )
                 ),
                 tabPanel("Datatable comparison View",
                          # display tables if user select to
                          fluidRow(
                            box(
                              title = "Original Data",
                              status = "primary", 
                              solidHeader = TRUE,
                              width = 12, 
                              DT::dataTableOutput(ns('table_original'))
                            ),
                            
                            box(
                              title = "Processed Data",
                              status = "primary", 
                              solidHeader = TRUE,
                              width = 12, 
                              DT::dataTableOutput(ns('table_processed'))
                            )
                          )
                          
                 )
                 
                 
               )
      ),
    shinysky::busyIndicator(text = "Processing, please wait ...",
                            img = "ajaxloaderq.gif",
                            wait = 500)
  )
  
}


datatableProcess_full_server <- function(input, output, session, data_table) {
  ns <- session$ns
  
  # display original table
  output$table_original <- DT::renderDataTable(
    data_table,
    extensions = c('Buttons'),
    #selection = list(target = "row+column"),
    options = list(
      dom = 'Bfrtip',
      pageLength = 5,
      scrollX = TRUE,
      buttons = I('colvis')
      #buttons = list(list(extend = 'colvis', columns = 1:2))
    )
  )
  
  
  # generate ui for row and column selection
  observe({
    # only if the box is checked
    if(input$manual_select_row_column){
      output$ui_selectInput_select_column <- renderUI({
        column_index <- 1:ncol(data_table)
        names(column_index) <- colnames(data_table)
        selectizeInput(ns("keep_column_index"),
                       "Columns to keep:",
                       column_index,
                       multiple = TRUE,
                       options = list(plugins=list('drag_drop','remove_button'))
        )
      })
      output$ui_selectInput_select_row <- renderUI({
        row_index <- 1:nrow(data_table)
        names(row_index) <- rownames(data_table)
        selectizeInput(ns("keep_row_index"),
                       "Rows to keep:",
                       row_index,
                       multiple = TRUE,
                       options = list(plugins=list('drag_drop','remove_button'))
        )
        
      })
    }
    
    
    # 
    if(input$RegExp_select_row){
      my_rownames <- rownames(data_table)
      output$ui_selectInput_available_rownames <- renderUI({
        selectizeInput(ns("Available_rownames"),
                       "Available Row names:",
                       my_rownames
        )
      })
    }
    if(input$RegExp_select_column){
      my_colnames <- colnames(data_table)
      output$ui_selectInput_available_colnames <- renderUI({
        selectizeInput(ns("Available_colnames"),
                       "Available Column names:",
                       my_colnames
        )
      })
    }
    
    
  }) 
   
  # start processing
  data_table_processed <- reactive({
    
    # plot overall profile before processing
    output$heatmap_before_process  <- renderPlot({
      heatmap(as.matrix(data_table), Rowv = NA, Colv = NA, main = "Profile before Process")
    })
    
    output$density_before_process  <- renderPlot({
      plot(density(as.matrix(data_table), na.rm =  TRUE), main = "Densityplot before Process ")
    })
    
    
    
    if( input$button_apply_processing == 0 ){ # set up the dependency on the button
      
      # generat ui
      output$ui_plot_profile_comparison <- renderUI({
        box(
          title = "Data matrix profiling",
          width = 12,
          status = "primary",
          solidHeader = TRUE,
          
          column(6,
                 plotOutput(ns("heatmap_before_process"))
                 
          ),
          column(6,
                 plotOutput(ns("density_before_process"))
          )
          
        )
        
      })
      
      
      return(data_table)
    }else{
      isolate({
        
        
        # initialize the output text for analysis summary
        process_summary <- paste0("In the orginal data matrix", "\n",
                                  "Number of columns:", ncol(data_table), "\n",
                                  "Number of rows:", nrow(data_table), "\n"
        )
        # process_summary <- paste0(process_summary, "\n",
        #                           summary(data_table))
        # _manual row and column selection 
        if(input$manual_select_row_column){
          
          # do filtering
          if (length(input$keep_row_index)) {
            rows_keep <- as.numeric(input$keep_row_index)
          }else{
            rows_keep <- 1:nrow(data_table)
          }
          #print(rows_keep)
          
          if(length(input$keep_column_index)){
            columns_keep <- as.numeric(input$keep_column_index)
            data_processed <- data_table[rows_keep, columns_keep, drop = FALSE]
          }else{
            columns_keep <- 1:ncol(data_table)
            data_processed <- data_table[rows_keep, , drop = FALSE]
          }
          
          # format the summary text
          outputtext <- paste0("Rows Selected: ", toString(rows_keep), "\n",
                               "Columns Selected: ", toString(columns_keep)
          )
          process_summary <- paste(process_summary,outputtext, sep ="\n")
          
        }else{
          data_processed <- data_table
          
        }
        # _pattern row selection 
        
        if(input$RegExp_select_row){
          
          data_processed <- as.data.frame(t(data_processed))
          #choices = list("starts with", "ends with","contains"),
          data_processed <- switch(input$regexp_type_row,
                                   
                                   
                                   "starts with" = {
                                     t(dplyr::select(data_processed, starts_with(input$regexp_value_row)))
                                   },
                                   "ends with" = {
                                     t(dplyr::select(data_processed, ends_with(input$regexp_value_row)))
                                   },
                                   "contains" = {
                                     t(dplyr::select(data_processed, contains(input$regexp_value_row)))
                                   }
                                   
          )
          
        }
        
        # _pattern column selection -
        
        if(input$RegExp_select_column){
          
          #choices = list("starts with", "ends with","contains"),
          data_processed <- switch(input$regexp_type_column,
                                   
                                   "starts with" = {
                                     dplyr::select(data_processed, starts_with(input$regexp_value_column))
                                   },
                                   "ends with" = {
                                     dplyr::select(data_processed, ends_with(input$regexp_value_column))
                                   },
                                   "contains" = {
                                     dplyr::select(data_processed, contains(input$regexp_value_column))
                                   }
                                   
          )
          
        }
        
        
        
        
        # _missingvalue_filtering 
        if(input$missingvalue_filtering){
          
          #process_summary$missingvalue_filtering <- "yes"
          
          for(NA_type in input$matrix_filter_NA_type){
            data_processed[data_processed == NA_type] <- NA
          }
          
          
          result_mising_value_filtering <- missingvalue_filtering(data_processed, threshold = input$missingvalue_filtering_q)
          
          data_processed <-result_mising_value_filtering$data_qualified
          data_filtered_out <-result_mising_value_filtering$filtering_summary$data_not.qualified
          
          data_filtering_number_qualified <- result_mising_value_filtering$filtering_summary$number.qualified
          data_filtering_number_notqualified <- result_mising_value_filtering$filtering_summary$number.not.qualified
          
          # output for the not qualified tables
          # 
          #print(head(data_filtered_out))
          output$table_filtered_out <- DT::renderDataTable(
            data_filtered_out,
            extensions = c('Buttons'),
            options = list(
              dom = 'Bfrtip',
              pageLength = 5,
              scrollX = TRUE,
              buttons = I('colvis')
              #buttons = list(list(extend = 'colvis', columns = 1:2))
            )
          )

          output$ui_table_filtered_out <- renderUI({
            box(
              title = "Data filtered out/not qulified",
              status = "primary",
              solidHeader = TRUE,
              width = 12,
              DT::dataTableOutput(ns('table_filtered_out'))
            )
          })
          
          
          # format the summary text
          outputtext <- paste0("Missing value impuation:  Yes", "\n",
                               "\tQ value: ",input$missingvalue_filtering_q, "\n",
                               "Numer of rows qualified: ", data_filtering_number_qualified, "\n",
                               "Numer of rows not qualified: ",data_filtering_number_notqualified
          )
          process_summary <- paste(process_summary,outputtext, sep ="\n")
        }
        

        
        # _missing value imputation
        if(input$matrix_inputation){
          
          for(NA_type in input$matrix_inputation_NA_type){
            data_processed[data_processed == NA_type] <- NA
          }
          
          
          if(input$imputation_by == "row"){
            data_processed_imp <- t(rrcovNA::impSeqRob(t(data_processed), alpha = input$matrix_inputation_alpha)$x)
            # reorder columns becase imputation by row might change column orders
            data_processed <- data_processed_imp[,match(colnames(data_processed),colnames(data_processed_imp))]
          }else{
            data_processed_imp <- rrcovNA::impSeqRob(data_processed, alpha = input$matrix_inputation_alpha)$x
            # reorder rows becase imputation by column might change row orders
            data_processed <- data_processed_imp[match(rownames(data_processed),rownames(data_processed_imp)),]
          }
          
          outputtext <- paste0("Data imputation:  Yes", "\n",
                               "Imputation by: ", input$imputation_by, "\n",
                               "Alpha value of imputation: ",input$matrix_inputation_alpha 
          )
          process_summary <- paste(process_summary,outputtext, sep ="\n")
          
        }
        # _log transformation-
        # 20190827 - changed the log transform to "data_processed + 1" for volcano plot only
        if(input$matrix_transform_switch){
          data_processed <- switch(input$log_transform,
                                   "log10 tranform" = {log10(data_processed+1)},
                                   "log2 tranform" = {log2(data_processed+1)},
                                   "ln tranform" = {log(data_processed+1)}
          )
          
          outputtext <- paste0("Log transformation: YES", "\n",
                               "Transformation type: ", input$log_transform
          )
          process_summary <- paste(process_summary,outputtext, sep ="\n")
          
        }
        # _do scale on column or row
        if(input$scale_table){
          data_processed_before_scaling <- data_processed
          switch(input$scale_by,
                 "row" = {
                   # do scaling
                   data_processed <- t(scale(t(data_processed), 
                                               center =  input$scale_center,
                                               scale =  input$scale_scale
                   ))
                   plot_maintitle_before <- "Before Scaling on Row"
                   plot_maintitle_after <- "After Scaling on Row"
                   

                   
                 },
                 "column" = {
                   # do scaling
                   data_processed <- scale(data_processed,
                                           center =  input$scale_center,
                                           scale =  input$scale_scale
                   )
                   plot_maintitle_before <- "Before Scaling on Column"
                   plot_maintitle_after <- "After Scaling on Column"

                 },
                 "row then column" = {
                   
                   data_processed <- t(scale(t(data_processed), 
                                               center =  input$scale_center,
                                               scale =  input$scale_scale
                   ))
                   data_processed <- scale(data_processed,
                                           center =  input$scale_center,
                                           scale =  input$scale_scale
                   )
                   plot_maintitle_before <- "Before Scaling on Row and Column"
                   plot_maintitle_after <- "After Scaling on Row and Column"
                 },
                 "column then row" = {
                   data_processed <- scale(data_processed,
                                           center =  input$scale_center,
                                           scale =  input$scale_scale
                   )
                   data_processed <- t(scale(t(data_processed), 
                                               center =  input$scale_center,
                                               scale =  input$scale_scale
                   ))
                   plot_maintitle_before <- "Before Scaling on Column and Row"
                   plot_maintitle_after <- "After Scaling on Column and Row"
                   
                 }
          )
          
          
          # plot beofore scaling
          output$boxplot_before_scale  <- renderPlot({
            boxplot(data_processed_before_scaling,main = plot_maintitle_before)
          })
          
          
          # plot after scaling
          output$boxplot_after_scale  <- renderPlot({
            boxplot(data_processed,main = plot_maintitle_after)
          })

          outputtext <- paste0("Scale on: ",input$scale_by, "\n",
                               "If center: ", input$scale_center,"\n",
                               "If sd normalize:  ", input$scale_scale
          )
          process_summary <- paste(process_summary,outputtext, sep ="\n")
          
          
          # generat ui
          output$ui_boxplot_check_scale <- renderUI({
            box(
              title = "Check the scale effect",
              width = 12,
              status = "primary",
              solidHeader = TRUE,
              
              column(6,
                     plotOutput(ns("boxplot_before_scale"))
              ),
              column(6,
                     plotOutput(ns("boxplot_after_scale"))
              )
              
            )
            
          })

          
        }
        # _reverse table
        if(input$Reverse_table){
          if(input$reverse_minus_value){
            data_processed <- -data_processed
          }
          
          if(input$reverse_reciprocal_value){
            data_processed <- 1/data_processed
          }
          
          outputtext <- paste0("If reverse table: ","yes", "\n",
                               "If minus table:  ", input$reverse_minus_value, "\n",
                               "If reciprocal  ", input$reverse_reciprocal_value
          )
          process_summary <- paste(process_summary,outputtext, sep ="\n")
          
          
        }
       
        # _transpose
        if(input$transpose_table){
          data_processed <- t(data_processed)
          
          outputtext <- paste0("Transpose data matrix: Yes")
          process_summary <- paste(process_summary,outputtext, sep ="\n")
        }
        
        
        # plot overall profile after processing
        output$heatmap_after_process  <- renderPlot({
          heatmap(as.matrix(data_processed), Rowv = NA, Colv = NA, main = "Profile after Process")
        })
        
        output$density_after_process  <- renderPlot({
          plot(density(as.matrix(data_processed), na.rm =  TRUE), main = "Densityplot after Process ")
        })
        
        # generat ui
        output$ui_plot_profile_comparison <- renderUI({
          box(
            title = "Process Effect",
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            
            column(6,
                   plotOutput(ns("heatmap_before_process")),
                   plotOutput(ns("density_before_process"))
            ),
            column(6,
                   plotOutput(ns("heatmap_after_process")),
                   plotOutput(ns("density_after_process"))
            )
            
          )
          
        })
        
        # _summarize data process settings ---
        
        process_summary <- paste0(process_summary, "\n",
                                  "In the Processed data matrix", "\n",
                                  "Number of columns:", ncol(data_processed), "\n",
                                  "Number of rows:", nrow(data_processed), "\n"
        )
        
        output$process_summary <- renderPrint({ 
          cat(process_summary)
        })
        
        output$ui_process_summary <- renderUI({
          box(
            title = "Data process summary",
            status = "primary", 
            solidHeader = TRUE,
            width = 12, 
            verbatimTextOutput(ns("process_summary"))
            #helpText("test")
          )
        })
        
        
        callModule(tableDownload,"download_processed_table", data = data_processed, filename_tag = "Processed_dt") 
        callModule(plaintextDownload,"download_process_summary", data = process_summary, filename_tag = "Report_summary")
        
        output$ui_processedata_download <- renderUI({
          box(
            #title = "Data process summary",
            status = "primary", 
            solidHeader = TRUE,
            width = 12, 
            #verbatimTextOutput(ns("process_summary"))
            tableDownloadUI(ns("download_processed_table"), # make sure to use ns, because this is moduel in module,
                            label = "Download Processed Data table"),
            plaintextDownloadbutton(ns("download_process_summary"), # make sure to use ns, because this is moduel in module,
                                    label = "Download process summary")
            
            #helpText("test")
          )
        })
        
        
        
        # _display the processed table-
        output$table_processed <- DT::renderDataTable(
          data_processed,
          extensions = c('Buttons'),
          #selection = list(target = "row+column"),
          options = list(
            dom = 'Bfrtip',
            pageLength = 5,
            scrollX = TRUE,
            buttons = I('colvis')
            #buttons = list(list(extend = 'colvis', columns = 1:2))
          )
        )
        
        data_processed
        #return(mtcars)
        
      }) # isolate ends
    }
    
  }) 
  
  data_table_processed

  
}



datatableProcess_UI <- function(id,boxtitle,boxwidth) {
  
  install.packages.auto(dplyr)
  
  ns <- NS(id)
  
  box(
    title = boxtitle,
    width = boxwidth,
    solidHeader = TRUE,
    status = "primary",
    collapsible = TRUE,
    
    # process settings
    column(4,
           # show or hide tables
           
           checkboxInput(ns("Show_original_table"),label = "Show original data table", FALSE),
           checkboxInput(ns("Show_processed_table"),label = "Show processed data table", FALSE),
           
           
           # ui for manual selection   
           
           checkboxInput(ns('manual_select_row_column'), 'Manual selection of rows and columns?', FALSE),
           #bsTooltip(ns("manual_select_row_column"), "Note that maunal selelction Works better for small tables",
           #          "right"),
           
           conditionalPanel(
             condition = paste0("input['", ns("manual_select_row_column"), "']"),
             box(
               width =12,
               status = "primary",
               solidHeader = TRUE,
               column(6, uiOutput(ns("ui_selectInput_select_row"))),
               column(6, uiOutput(ns("ui_selectInput_select_column")))
             )
             
           ),
           
           # ui for selection by pattern for rows -
           
           checkboxInput(ns('RegExp_select_row'), 'Name-Pattern selection of rows?', FALSE),
           
           conditionalPanel(
             condition = paste0("input['", ns("RegExp_select_row"), "']"),
             wellPanel(
               selectInput(ns("regexp_type_row"), "Pattern type:",
                           choices = list("starts with", "ends with","contains"),
                           selected = "starts with"),
               textInput(ns("regexp_value_row"), "Type the text to match", value = "e.g. sample_")
               
             )
           ),
           
           # ui for selection by pattern for columns  
           
           checkboxInput(ns('RegExp_select_column'), 'Name-Pattern selection of columns?', FALSE),
           
           conditionalPanel(
             condition = paste0("input['", ns("RegExp_select_column"), "']"),
             wellPanel(
               selectInput(ns("regexp_type_column"), "Pattern type:",
                           choices = list("starts with", "ends with","contains"),
                           selected = "starts with"),
               textInput(ns("regexp_value_column"), "Type the text to match", value = "e.g. variable_")
               
             )
           ),
           
           # ui for filtering out missing values  
           checkboxInput(ns('missingvalue_filtering'), 'Filtering out rows with missing values?', FALSE),
           
           conditionalPanel(
             condition = paste0("input['", ns("missingvalue_filtering"), "']"),
             wellPanel(
               sliderInput(ns("missingvalue_filtering_q"), label = "Q value of presence", min = 0,
                           max = 1, value = 0.75),
               helpText("\tQ vaule is the presence percentage threshold. For example, 0,75 means 75% of presence across all samples")
               # bsTooltip(ns("missingvalue_filtering_q"), "Q vaule is the presence percentage threshold",
               #           "right", options = list(container = "body"))
             )
           ),
           
           
           # ui for log transformation  
           checkboxInput(ns('matrix_transform_switch'), 'Log transform the whole data matrix?', FALSE),
           
           conditionalPanel(
             condition = paste0("input['", ns("matrix_transform_switch"), "']"),
             wellPanel(
               selectInput(ns("log_transform"), "Log tranform?",
                           choices = list("log10 tranform", "log2 tranform","ln tranform"),
                           selected = "log10 tranform")
               
             )
             
           ),
           
           # ui for missing value imputation  
           
           checkboxInput(ns('matrix_inputation'), 'Sequential missing value impuation?', FALSE),
           conditionalPanel(
             condition = paste0("input['", ns("matrix_inputation"), "']"),
             wellPanel(
               selectInput(ns("matrix_inputation_NA_type"),
                           "Type of NA value in data",
                           c("NA", "0", "Inf")),
               selectInput(ns("imputation_by"), 
                           "Imnputation by:",
                           choices = c("row","column" ), 
                           selected = "row"),
               sliderInput(ns("matrix_inputation_alpha"), label = "Alpha/Strength:", min = 0,
                           max = 1, value = 0.9)
             )
             
           ),
           
           # # ui for scaling  
           
           checkboxInput(ns('scale_table'), 'Scale/Z-score/Normalize table?', FALSE),
           conditionalPanel(
             condition = paste0("input['", ns("scale_table"), "']"),
             wellPanel(
               selectInput(ns("scale_by"),
                           "Scale on:",
                           choices = c("row","column" ),
                           selected = "row"),
               helpText("\tThis function centers and scales numeric matrix. ",
                        "Center means the data(row or column)'s mean is going to be 0. ",
                        "Scale is done after centering. Scale is done by divding each value by their standard deviation.",
                        "This function is called z-score some elsewhere. "
               )
               
               
             )
           ),
           # ui for transpose  
           checkboxInput(ns('transpose_table'), 'Transpose table?', FALSE),
           
           
           
           # apply button
           actionButton(ns("button_apply_processing"), 
                        icon = icon("paper-plane"),
                        label = "apply Selected Procedure(s)",
                        style="float:left; color: #fff; background-color: #337ab7; border-color: #2e6da4"
           )
           
    ),
    
    #ui for result summary   
    column(8,
           #display summary
           uiOutput(ns("ui_process_summary")),
           uiOutput(ns("ui_processedata_download")),
           # display check scale plot if data is scaled
           uiOutput(ns("ui_boxplot_check_scale")),
           
           
           # display tables if user select to
           conditionalPanel(
             condition = paste0("input['", ns("Show_original_table"), "']"),
             box(
               title = "Original Data",
               status = "primary", 
               solidHeader = TRUE,
               width = 12, 
               DT::dataTableOutput(ns('table_original'))
             )
           ),
           
           conditionalPanel(
             condition = paste0("input['", ns("Show_processed_table"), "']"),
             box(
               title = "Processed Data",
               status = "primary", 
               solidHeader = TRUE,
               width = 12, 
               DT::dataTableOutput(ns('table_processed'))
             )
           )
    ),
    shinysky::busyIndicator(text = "Processing, please wait ...",
                            img = "ajaxloaderq.gif",
                            wait = 500)

  )
  
}


datatableProcess <- function(input, output, session, data_table) {
  ns <- session$ns
  
  output$table_original <- DT::renderDataTable(
    data_table,
    extensions = c('Buttons'),
    #selection = list(target = "row+column"),
    options = list(
      dom = 'Bfrtip',
      pageLength = 5,
      scrollX = TRUE,
      buttons = I('colvis')
      #buttons = list(list(extend = 'colvis', columns = 1:2))
    )
  )
  
  # start data processing -
  
  data_table_processed <- reactive({
    
    # only if the box is checked
    if(input$manual_select_row_column){
      output$ui_selectInput_select_column <- renderUI({
        column_index <- 1:ncol(data_table)
        names(column_index) <- colnames(data_table)
        selectInput(ns("keep_column_index"),
                    "Columns to keep:",
                    column_index,
                    multiple = TRUE)
        
      })
      output$ui_selectInput_select_row <- renderUI({
        row_index <- 1:nrow(data_table)
        names(row_index) <- rownames(data_table)
        selectInput(ns("keep_row_index"),
                    "Rows to keep:",
                    row_index,
                    multiple = TRUE)
        
      })
    }
    
    
    if( input$button_apply_processing == 0 ){ # set up the dependency on the button
      return(data_table)
    }else{
      isolate({
        
        # initialize the output text for analysis summary
        process_summary <- paste0("In the orginal data matrix", "\n",
                                  "Number of columns:", ncol(data_table), "\n",
                                  "Number of rows:", nrow(data_table), "\n"
        )
        
        # _manual row and column selection 
        if(input$manual_select_row_column){
          
          # do filtering
          if (length(input$keep_row_index)) {
            rows_keep <- as.numeric(input$keep_row_index)
          }else{
            rows_keep <- 1:nrow(data_table)
          }
          #print(rows_keep)
          
          if(length(input$keep_column_index)){
            columns_keep <- as.numeric(input$keep_column_index)
            data_processed <- data_table[rows_keep, columns_keep, drop = FALSE]
          }else{
            columns_keep <- 1:ncol(data_table)
            data_processed <- data_table[rows_keep, , drop = FALSE]
          }
          
          # format the summary text
          outputtext <- paste0("Rows Selected: ", toString(rows_keep), "\n",
                               "Columns Selected: ", toString(columns_keep)
          )
          process_summary <- paste(process_summary,outputtext, sep ="\n")
          
        }else{
          data_processed <- data_table
          
        }
        # _pattern row selection 
        
        if(input$RegExp_select_row){
          
          data_processed <- as.data.frame(t(data_processed))
          #choices = list("starts with", "ends with","contains"),
          data_processed <- switch(input$regexp_type_row,
                                   
                                   
                                   "starts with" = {
                                     t(dplyr::select(data_processed, starts_with(input$regexp_value_row)))
                                   },
                                   "ends with" = {
                                     t(dplyr::select(data_processed, ends_with(input$regexp_value_row)))
                                   },
                                   "contains" = {
                                     t(dplyr::select(data_processed, contains(input$regexp_value_row)))
                                   }
                                   
          )
          
        }
        
        # _pattern column selection 
        
        if(input$RegExp_select_column){
          
          #choices = list("starts with", "ends with","contains"),
          data_processed <- switch(input$regexp_type_column,
                                   
                                   "starts with" = {
                                     dplyr::select(data_processed, starts_with(input$regexp_value_column))
                                   },
                                   "ends with" = {
                                     dplyr::select(data_processed, ends_with(input$regexp_value_column))
                                   },
                                   "contains" = {
                                     dplyr::select(data_processed, contains(input$regexp_value_column))
                                   }
                                   
          )
          
        }
        
        
        
        
        # _missingvalue_filtering 
        if(input$missingvalue_filtering){
          
          #process_summary$missingvalue_filtering <- "yes"
          
          result_mising_value_filtering <- missingvalue_filtering(data_processed, threshold = input$missingvalue_filtering_q)
          
          data_processed <-result_mising_value_filtering$data_qualified
          data_filtered_out <-result_mising_value_filtering$filtering_summary$data_not.qualified
          
          data_filtering_number_qualified <- result_mising_value_filtering$filtering_summary$number.qualified
          data_filtering_number_notqualified <- result_mising_value_filtering$filtering_summary$number.not.qualified
          
          # output for the not qulified tables
          output$table_filtered_out <- DT::renderDataTable(
            data_filtered_out,
            extensions = c('Buttons'),
            #selection = list(target = "row+column"),
            options = list(
              dom = 'Bfrtip',
              pageLength = 5,
              scrollX = TRUE,
              buttons = I('colvis')
              #buttons = list(list(extend = 'colvis', columns = 1:2))
            )
          )
          
          # format the summary text
          outputtext <- paste0("Missing value impuation:  Yes", "\n",
                               "\tQ value: ",input$missingvalue_filtering_q, "\n",
                               "Numer of rows qualified: ", data_filtering_number_qualified, "\n",
                               "Numer of rows not qualified: ",data_filtering_number_notqualified
          )
          process_summary <- paste(process_summary,outputtext, sep ="\n")
        }
        
        # _log transformation
        # 20190827 - changed the log transform to "data_processed + 1" for volcano plot only
        if(input$matrix_transform_switch){
          data_processed <- switch(input$log_transform,
                                   "log10 tranform" = {log10(data_processed+1)},
                                   "log2 tranform" = {log2(data_processed+1)},
                                   "ln tranform" = {log(data_processed+1)}
          )
          
          outputtext <- paste0("Log transformation: YES", "\n",
                               "Transformation type: ", input$log_transform
          )
          process_summary <- paste(process_summary,outputtext, sep ="\n")
          
        }
        
        # _missing value imputation
        if(input$matrix_inputation){
          data_processed[data_processed == input$matrix_inputation_NA_type] <- NA
          if(input$imputation_by == "row"){
            data_processed_imp <- t(rrcovNA::impSeqRob(t(data_processed), alpha = input$matrix_inputation_alpha)$x)
            # reorder columns becase imputation by row might change column orders
            data_processed <- data_processed_imp[,match(colnames(data_processed),colnames(data_processed_imp))]
          }else{
            data_processed_imp <- rrcovNA::impSeqRob(data_processed, alpha = input$matrix_inputation_alpha)$x
            # reorder rows becase imputation by column might change row orders
            data_processed <- data_processed_imp[match(rownames(data_processed),rownames(data_processed_imp)),]
          }
          
          outputtext <- paste0("Data imputation:  Yes", "\n",
                               "Imputation by: ", input$imputation_by, "\n",
                               "Alpha value of imputation: ",input$matrix_inputation_alpha 
          )
          process_summary <- paste(process_summary,outputtext, sep ="\n")
          
        }
        
        # _do scale on column or row
        if(input$scale_table){
          if(input$scale_by == "row"){
            
            data_processed_before_scaling <- data_processed
            # plot beofore scaling
            output$boxplot_before_scale  <- renderPlot({
              boxplot(t(data_processed_before_scaling),main = "Before Scaling on Row")
            })
            
            # do scaling
            data_processed <- t(scale(t(data_processed)))
            
            # plot after scaling
            output$boxplot_after_scale  <- renderPlot({
              boxplot(t(data_processed),main = "After Scaling on Row")
            })
            
          }else{
            # plot beofore scaling
            data_processed_before_scaling <- data_processed
            output$boxplot_before_scale  <- renderPlot({
              boxplot(data_processed_before_scaling,main = "Before Scaling on Column")
            })
            
            # do scaling
            data_processed <- scale(data_processed)
            
            # plot after scaling
            output$boxplot_after_scale  <- renderPlot({
              boxplot(data_processed,main = "After Scaling on Column")
            })
            
          }
          
          # generat ui
          output$ui_boxplot_check_scale <- renderUI({
            box(
              title = "Check the scale effect",
              width = 12,
              status = "primary",
              solidHeader = TRUE,
              
              column(6,
                     plotOutput(ns("boxplot_before_scale"))
              ),
              column(6,
                     plotOutput(ns("boxplot_after_scale"))
              )
              
            )
            
          })
          
          outputtext <- paste0("Scale matrix: Yes ", "\n",
                               "Scale by: ",input$scale_by 
          )
          process_summary <- paste(process_summary,outputtext, sep ="\n")
          
        }

        
        # _transpose
        if(input$transpose_table){
          data_processed <- t(data_processed)
          
          outputtext <- paste0("Transpose data matrix: Yes")
          process_summary <- paste(process_summary,outputtext, sep ="\n")
        }
        
        # _summarize data process settings 
        
        process_summary <- paste0(process_summary, "\n",
                                  "In the Processed data matrix", "\n",
                                  "Number of columns:", ncol(data_processed), "\n",
                                  "Number of rows:", nrow(data_processed), "\n"
        )
        
        output$process_summary <- renderPrint({ 
          cat(process_summary)
        })
        
        output$ui_process_summary <- renderUI({
          box(
            title = "Data process summary",
            status = "primary", 
            solidHeader = TRUE,
            width = 12, 
            verbatimTextOutput(ns("process_summary"))
            #helpText("test")
          )
        })
        
        
        callModule(tableDownload,"download_processed_table", data = data_processed, filename_tag = "Processed_dt") 
        callModule(plaintextDownload,"download_process_summary", data = process_summary, filename_tag = "Report_summary")
        
        output$ui_processedata_download <- renderUI({
          box(
            #title = "Data process summary",
            status = "primary", 
            solidHeader = TRUE,
            width = 12, 
            #verbatimTextOutput(ns("process_summary"))
            tableDownloadUI(ns("download_processed_table"), # make sure to use ns, because this is moduel in module,
                            label = "Download Processed Data table"),
            plaintextDownloadbutton(ns("download_process_summary"), # make sure to use ns, because this is moduel in module,
                                    label = "Download process summary")
            
            #helpText("test")
          )
        })
        
        
        
        # _display the processed table 
        output$table_processed <- DT::renderDataTable(
          data_processed,
          extensions = c('Buttons'),
          #selection = list(target = "row+column"),
          options = list(
            dom = 'Bfrtip',
            pageLength = 5,
            scrollX = TRUE,
            buttons = I('colvis')
            #buttons = list(list(extend = 'colvis', columns = 1:2))
          )
        )
        
        data_processed
        #return(mtcars)
        
      }) # isolate ends
    }
    
  }) 
  
  # return values
  data_table_processed
  #return(mtcars)
  
  # return(list(
  #   data_table_processed = rvalues$data_processeddata_processed
  #   #"summary" = process_summary
  # )
  # )
  
}



# Valid colors are: red, yellow, aqua, blue, light-blue, 
# green, navy, teal, olive, lime, orange, fuchsia, purple, maroon, black.

# ui function
# this name_tag is for labeling the down load file name
# plot_type = c("ggplot2","plotly", "recordPlot")



plotInBox_UI <- function(id, boxtitle = NULL,boxwidth = 6, plot_height = 800) {
  ns <- NS(id)
  box(
    title = boxtitle,
    width = boxwidth,
    solidHeader = TRUE,
    status = "primary",
    # box for plotting
    uiOutput(ns("plot_in_box")),
    
    # box for options
    box(
      width = 12,
      solidHeader = TRUE,
      #checkboxInput(ns("display"), "Show Value"),
      checkboxInput(ns("Show_plot_options"),"Export Plot", FALSE),
      
      conditionalPanel(
        condition = paste0("input['", ns("Show_plot_options"), "']"),
        box(
          title = "Resize noninteractive plot",
          solidHeader = TRUE,
          status = "primary",
          width = 6,
          sliderInput(ns("plot_width"), "Plot Width", 0, 100, 100),
          sliderInput(ns("plot_height"), "Plot Height", 4, 2000, plot_height),
          actionButton(ns("replot"),
                       icon = icon("refresh"),
                       label = "Replot"
                       
          )
        ),
        box(
          title = "Export plot",
          solidHeader = TRUE,
          status = "primary",
          width = 6,
          textInput(inputId= ns("export_width"), label="width (inch)", value = 10),
          textInput(inputId= ns("export_height"), label="height (inch)", value = 8),
          textInput(inputId= ns("pointsize"), label="Text size", value = 12),
          textInput(inputId= ns("resolution"), label="PPI for PNG", value = 300),
          
          downloadButton(ns("download_SVG"), 'SVG'),
          downloadButton(ns("download_PDF"), 'PDF'),
          downloadButton(ns("download_PNG"), 'PNG')
        )
        
      )
      
    )
    
  )
}

plotInBox <- function(input, output, session, 
                      plot_object, plot_type, name_tag, 
                      interactive = TRUE) {
  ns <- session$ns
  
  observe({
    if(plot_type == "ggplot2"){
      
      if(interactive){ # default is interactive plotting
        output$plot <- renderPlotly({
          ggplotly(plot_object)
        })
        
      }else{ # regular gglolt2 output when intarective set to FALSE
        output$plot <- renderPlot({
          plot_object
        })
        
      }
      
      
      # for SVG download
      output$download_SVG <- downloadHandler(
        filename <- paste0(name_tag,".svg"), 
        content <- function(file) {
          svg(file,
              width = as.numeric(input$export_width), 
              height = as.numeric(input$export_height),
              pointsize = as.numeric(input$pointsize),
              bg = "white")
          print(plot_object) # here it has to be "print", "plot" works in some rare cases
          #plot_object
          dev.off()
        }
      )
      # for high resolution PDF download
      output$download_PDF <- downloadHandler(
        
        filename <- paste0(name_tag,".pdf"), 
        content <- function(file) {
          pdf(file,
              width = as.numeric(input$export_width), 
              height = as.numeric(input$export_height), 
              pointsize = as.numeric(input$pointsize),
              bg = "white")
          
          #plot_object
          print(plot_object) # here it has to be "print", "plot" works in some rare cases
          dev.off()
        }
      )
      # for high resolution PNG download
      output$download_PNG <- downloadHandler(
        
        filename <- paste0(name_tag,".png"), 
        content <- function(file) {
          png(file,res = as.numeric(input$resolution),
              width = as.numeric(input$export_width), 
              height = as.numeric(input$export_height), 
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
        #print(plot_object)
        replayPlot(plot_object)
      })
      
      # for SVG download
      output$download_SVG <- downloadHandler(
        filename <- paste0(name_tag,".svg"), 
        content <- function(file) {
          svg(file,
              width = as.numeric(input$export_width), 
              height = as.numeric(input$export_height),
              pointsize = as.numeric(input$pointsize),
              bg = "white")
          # this is important, it has to be print, not plot, or nothing, 
          # because, the object is already a plot
          print(plot_object)
          dev.off()
        }
      )
      # for high resolution PDF download
      output$download_PDF <- downloadHandler(
        
        filename <- paste0(name_tag,".pdf"), 
        content <- function(file) {
          pdf(file,
              width = as.numeric(input$export_width), 
              height = as.numeric(input$export_height), 
              pointsize = as.numeric(input$pointsize),
              bg = "white")
          
          #plot_object
          print(plot_object) # here it has to be "print", "plot" works in some rare cases
          dev.off()
        }
      )
      # for high resolution PNG download
      output$download_PNG <- downloadHandler(
        
        filename <- paste0(name_tag,".png"), 
        content <- function(file) {
          png(file,res = as.numeric(input$resolution),
              width = as.numeric(input$export_width), 
              height = as.numeric(input$export_height), 
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
  
  
  
  # put the plot into UI
  output$plot_in_box <- renderUI({
    
    box(
      width = 12,
      solidHeader = TRUE,
      
      
      if(plot_type == "ggplot2"){
        if(interactive){
          plotlyOutput(ns("plot"), 
                       width = paste0(input$plot_width, "%"),
                       height = paste0(input$plot_height, "px"))
        }else{ # only statically show ggplot2 object
          plotOutput(ns("plot"), 
                     width = paste0(input$plot_width, "%"),
                     height = paste0(input$plot_height, "px"))
        }
        
        
      }else if(plot_type == "plotly"){
        plotlyOutput(ns("plot"), 
                     width = paste0(input$plot_width, "%"),
                     height = paste0(input$plot_height, "px"))
      }else if(plot_type == "recordPlot"){
        plotOutput(ns("plot"), 
                   width = paste0(input$plot_width, "%"),
                   height = paste0(input$plot_height, "px"))
      }else{
        print("wrong plot_type, continue using recordPlot")
        plotOutput(ns("plot"), 
                   width = paste0(input$plot_width, "%"),
                   height = paste0(input$plot_height, "px"))
      }
      
    )
    
  })
  
  #reactive({input$slider + 5})
}





BoxSVGPNGUI <- function(id, 
                        box_title = NULL, 
                        plot_type = "ggplot2", 
                        collapsed = FALSE,
                        collapsible = FALSE,
                        box_width = 6, 
                        plot_width = "100%", 
                        plot_height = "500px"){
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
      
      checkboxInput(ns("Show_export_options"),"Export Plot", FALSE),
      
      # show if the checkbox checked
      # only show export menu for ggplot2 and recordPlot type
      if(plot_type == "ggplot2"| plot_type == "recordPlot"){
        tagList(
          conditionalPanel(
            condition = paste0("input['", ns("Show_export_options"), "']"),
            # box(
            #   title = "Resize plot",
            #   solidHeader = TRUE,
            #   status = "primary",
            #    width = 6,
            #    textInput(inputId = ns("plot_width"), label="width (inch)", value = 10),
            #    textInput(inputId = ns("plot_height"), label="height (inch)", value = 8),
            #     actionButton(ns("re-plot"), 
            #                  icon = icon("refresh"),
            #                  label = "Re-plot"
            #                  
            #     )
            # ),      
            box(
              title = "Export plot",
              solidHeader = TRUE,
              status = "primary",
              width = 6,
              textInput(inputId= ns("export_width"), label="width (inch)", value = 10),
              textInput(inputId= ns("export_height"), label="height (inch)", value = 8),
              textInput(inputId= ns("pointsize"), label="Text size", value = 12),
              textInput(inputId= ns("resolution"), label="PPI for PNG", value = 300),
              
              downloadButton(ns("download_SVG"), 'SVG'),
              downloadButton(ns("download_PDF"), 'PDF'),
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

BoxSVGPNG <-function(input, 
                     output, 
                     session, 
                     plot_object, 
                     name_tag, 
                     plot_type){
  
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
              width = as.numeric(input$export_width), 
              height = as.numeric(input$export_height),
              pointsize = as.numeric(input$pointsize),
              bg = "white")
          print(plot_object) # here it has to be "print", "plot" works in some rare cases
          #plot_object
          dev.off()
        }
      )
      # for high resolution PDF download
      output$download_PDF <- downloadHandler(
        
        filename <- paste0(name_tag,".pdf"), 
        content <- function(file) {
          pdf(file,
              width = as.numeric(input$export_width), 
              height = as.numeric(input$export_height), 
              pointsize = as.numeric(input$pointsize),
              bg = "white")
          
          #plot_object
          print(plot_object) # here it has to be "print", "plot" works in some rare cases
          dev.off()
        }
      )
      # for high resolution PNG download
      output$download_PNG <- downloadHandler(
        
        filename <- paste0(name_tag,".png"), 
        content <- function(file) {
          png(file,res = as.numeric(input$resolution),
              width = as.numeric(input$export_width), 
              height = as.numeric(input$export_height), 
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
        #print(plot_object)
        replayPlot(plot_object)
      })
      
      # for SVG download
      output$download_SVG <- downloadHandler(
        filename <- paste0(name_tag,".svg"), 
        content <- function(file) {
          svg(file,
              width = as.numeric(input$export_width), 
              height = as.numeric(input$export_height),
              pointsize = as.numeric(input$pointsize),
              bg = "white")
          # this is important, it has to be print, not plot, or nothing, 
          # because, the object is already a plot
          print(plot_object)
          dev.off()
        }
      )
      # for high resolution PDF download
      output$download_PDF <- downloadHandler(
        
        filename <- paste0(name_tag,".pdf"), 
        content <- function(file) {
          pdf(file,
              width = as.numeric(input$export_width), 
              height = as.numeric(input$export_height), 
              pointsize = as.numeric(input$pointsize),
              bg = "white")
          
          #plot_object
          print(plot_object) # here it has to be "print", "plot" works in some rare cases
          dev.off()
        }
      )
      # for high resolution PNG download
      output$download_PNG <- downloadHandler(
        
        filename <- paste0(name_tag,".png"), 
        content <- function(file) {
          png(file,res = as.numeric(input$resolution),
              width = as.numeric(input$export_width), 
              height = as.numeric(input$export_height), 
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





BoxTableUI <- function(id, name_tag = "", boxcollapsed = FALSE, boxwidth = 12){
  # Create a namespace function using the provided id
  ns <- NS(id)
  tagList(
    box(
      title = name_tag,
      status = "primary", 
      width = boxwidth,
      solidHeader = TRUE,
      collapsible = TRUE,
      collapsed = boxcollapsed,
      DT::dataTableOutput(ns("displaydatatable"))
    )
  )
}


BoxTable <-function(input, output, session, table_object, server = TRUE){
  
  output$displaydatatable <- DT::renderDataTable(
    table_object, 
    server = server, 
    filter = 'top', extensions = c('Buttons','Scroller'), 
    options = list(
      autoWidth = TRUE, 
      dom = 'Bfrtip',
      buttons = c('colvis'),
      scrollY = 200,
      scrollX = TRUE)
  ) 
  
  
}




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



## additional argument to define the label on the downloadButton
tableDownloadUI <- function(id, label = "Download CSV") {
  ns <- NS(id)
  
  downloadButton(ns("download_table"), label)
}

## allow users of the module to input a (reactive) data.frame to download as csv and a name for the file
tableDownload <- function(input, output, session, data, filename_tag = NULL
) {
  
  output$download_table <- downloadHandler(
    filename = function() {
      paste0(filename_tag,"_", Sys.time(), ".tsv")
    },
    content = function(file) {
      #write.csv(data, file)
      write.table(data,   # do not use data(),  here accepts either values or reactive expressions
                  file, 
                  sep = "\t",
                  col.names = NA)
    }
  )
}


## additional argument to define the label on the downloadButton
plaintextDownloadbutton <- function(id, label = "Download report") {
  ns <- NS(id)
  downloadButton(ns("download_report"), label)
}

## allow users of the module to input a (reactive) data.frame to download as csv and a name for the file
plaintextDownload <- function(input, output, session, data, filename_tag = "report_summary") {
  
  output$download_report <- downloadHandler(
    filename = function() {
      paste0(filename_tag,"_", Sys.time(), ".txt")
    },
    content = function(file) {
      write(data,   # do not use data(),  here accepts either values of reactive expressions
            file
            #file = "report.txt", 
      )
    }
  )
}

#install.packages("future.apply")

volcano_plot_UI <- function(id){
  ns <- NS(id)
  install.packages.auto(colourpicker)
  fluidRow(
    tabBox(
      width = 4,
      tabPanel("Statistics",
               strong("Select inputs for the comparison (Group1 vs Group2)"),
               uiOutput(ns("group_selection_1")),
               uiOutput(ns("group_selection_2")),
               selectInput(ns("Hypothesis"), "Hypothesis:",
                           c("Parametric", "Non-parametric"),
                           selected = "Parametric"
               ),
               selectInput(ns("p_adjust"), "Adjust p value:",
                           c("fdr", "bonferroni", "holm", "hochberg", "hommel", "none"),
                           selected = "none"
               ),
               numericInput(ns("p_cut_off"), "P-value cut-off", 0.05, 0.001, 1,  step = 0.001),
               numericInput(ns("fc_cut_off"), "Fold-change cut-off", 1.5,  1, 5, step = 0.1),
               selectInput(ns("threshold"), "Type of threshold:",
                           c("Smooth curve threshold" = "curve",
                             "Right-angle threshold" = "line"),
                           selected = "line"),
               conditionalPanel(
                 condition = paste0("input['", ns("threshold"), "']", " == 'curve' "),
                 numericInput(ns("curvature"), "Curvature", 0.05, 0.001, 1,  step = 0.001)
               ),
               selectInput(ns("show_intensity"), "Show intensity with point size?", 
                           c("Do not show intensity" = "no",
                             "Show average intensity of all samples" = "all",
                             "Show average intensity of Group1" = "1",
                             "Show average intensity of Group2" = "2"),
                           selected = "no"
               )
      ),
      tabPanel("Advanced",
               colourpicker::colourInput(
                 ns("color_picker_increase"), 
                 "Choose color for significantly increased features", 
                 "#ff6600"
               ),
               colourpicker::colourInput(
                 ns("color_picker_decrease"), 
                 "Choose color for significantly decreased features", 
                 "#3e64ff"
               ),
               colourpicker::colourInput(
                 ns("color_picker_ns"), 
                 "Choose color for non-significant features", 
                 "#f5f5f5"
               ),
               colourpicker::colourInput(
                 ns("color_picker_line"), 
                 "Choose color for line/curve", 
                 "#666666"
               ),
               sliderInput(
                 ns("alpha_dots"),
                 "Opacity of data points",
                 0.9, min = 0, max = 1, step = 0.05
               ),
               sliderInput(
                 ns("alpha_line"),
                 "Opacity of line/curve",
                 0.9, min = 0, max = 1, step = 0.05
               ),
               sliderInput(
                 ns("size_line"),
                 "Thickness of line/curve",
                 0.5, min = 0.1, max = 1, step = 0.1
               )
      )
    ),
    tabBox(
      width = 8,
      tabPanel("Volcano plot",
               conditionalPanel(
                 condition = paste0("output['", ns("analysisstatus"), "']"),
                 fluidRow(
                   shinydashboard::box(
                     width = 12,
                     solidHeader = TRUE,
                     align="center",
                     plotlyOutput(ns("volcano_plot_out"), width = "800px", height = "600px")
                   )
                 )
               )
      ),
      tabPanel("Result table download",
               downloadButton(outputId = ns("download_result_table"), label = "Download Result Table"),
               DT::dataTableOutput(ns('result_table'))
      )
      # ,
      # tabPanel("debug",
      #          # This is for debug
      #          box(
      #            width = 12,
      #            verbatimTextOutput(ns("debug"))
      #          )
      # )
    ),
    shinysky::busyIndicator(text = "Plotting, please wait ...",
                            img = "ajaxloaderq.gif",
                            wait = 500)
  )

}


volcano_plot_server <- function(input, output, session, data, meta){
  ns <- session$ns
  install.packages.auto(ggplot2)
  install.packages.auto(plotly)
  
  # Extract grouping information (group ids, group length)
  group_id <- reactive({ as.character(unique(meta[,1])) })
  group_length <- reactive({ as.integer(count(unique(meta)))  })
  
  # Passing reactive grouping info to selectInput choices
  outVar <- reactive({
    as.list(group_id())
  })
  output$group_selection_1 = renderUI({
    selectInput(ns("group_selection_input_1"), 'Please define your Group1', outVar(), outVar()[[1]])
  })
  output$group_selection_2 = renderUI({
    selectInput(ns("group_selection_input_2"), 'Please define your Group2', outVar(), outVar()[[2]])
  })
  
  # Separate data to Group1 and Group2 (data1 & data2)
  # Put group labels as column names
  data1 <- reactive({
             data1<- as.data.frame(subset(t(data), meta[,1] == input$group_selection_input_1))
             data1<-t(data1)
             colnames(data1)[]<- input$group_selection_input_1
             return(data1)
           })
  data2 <- reactive({
              data2<- as.data.frame(subset(t(data), meta[,1] == input$group_selection_input_2))
              data2<-t(data2)
              colnames(data2)[]<- input$group_selection_input_2
              return(data2)
           })
  
  # Combine as one table, store as data_combined
  data_combined <- reactive({ cbind(data1(), data2()) })

  stats_t <- reactive({
                stats_t <- future_lapply(1:nrow(data_combined()), function(x) t.test((data_combined()[x, 1:ncol(data1())]), (data_combined()[x, (ncol(data1()+1)):ncol(data_combined())])))
                names(stats_t) <- row.names(t(data_combined()))
                return(stats_t)
             })

  stats_wc <- reactive({
                stats_wc <- suppressWarnings(future_lapply(1:nrow(data_combined()), function(x) wilcox.test(data_combined()[x, 1:ncol(data1())], data_combined()[x, (ncol(data1())+1):ncol(data_combined())])))
                names(stats_wc) <- row.names(data_combined())
                return(stats_wc)
              })
  
  # Prepare p value/p adjust according to user selection, in the form of -log10(p) for plotting (as y-axis)
  log_p_as_y_axis <- reactive({
    if(input$Hypothesis == "Parametric"){
      if(input$p_adjust == "none"){
        p.value.t <- future_lapply(1:nrow(data_combined()), function(x) stats_t()[[x]]$p.value)
        names(p.value.t) <- row.names(data_combined())
        p.value.t <- t(as.matrix(as.data.frame(p.value.t)))
        log_p_val_t <- -log10(p.value.t)
        as.matrix(log_p_val_t)
      }else{
        p.value.t <- future_lapply(1:nrow(data_combined()), function(x) stats_t()[[x]]$p.value)
        names(p.value.t) <- row.names(data_combined())
        p.adj.t <- as.data.frame(p.adjust(p.value.t, method = input$p_adjust))
        p.adj.t <- as.matrix(p.adj.t)
        log_p_adj_t <- -log10(p.adj.t)
        as.matrix(log_p_adj_t)
      }
    }else if(input$Hypothesis == "Non-parametric"){
      if(input$p_adjust == "none"){
        p.value.wc <- future_lapply(1:nrow(data_combined()), function(x) stats_wc()[[x]]$p.value)
        names(p.value.wc) <- row.names(data_combined())
        p.value.wc <- t(as.matrix(as.data.frame(p.value.wc)))
        log_p_val_wc <- -log10(p.value.wc)
        as.matrix(log_p_val_wc)
      }else{
        p.value.wc <- future_lapply(1:nrow(data_combined()), function(x) stats_wc()[[x]]$p.value)
        names(p.value.wc) <- row.names(data_combined())
        p.adj.wc <- as.data.frame(p.adjust(p.value.wc, method = input$p_adjust))
        p.adj.wc <- as.matrix(p.adj.wc)
        log_p_adj_wc <- -log10(p.adj.wc)
        as.matrix(log_p_adj_wc)
      }
    }
  })
  
  
  # Calculate fold change
  log2FC <- reactive({
                      log2FC <- future_lapply(1:nrow(data_combined()), function(x) log2(mean(data_combined()[x, 1:ncol(data1())])/mean(data_combined()[x, (ncol(data1())+1):ncol(data_combined())])))
                      names(log2FC) <- row.names(data_combined())
                      as.matrix(t(as.data.frame(log2FC)))
    # data_combined()
            })
  
  # mean value of intensity for ggplot, if "no", all data points are the same size
  # users can customize point size according to the average value of all data, or control / treatment dataset
  intensity <- reactive({
    if(input$show_intensity == "no"){
      return(NULL)
    }else if(input$show_intensity == "all"){
      intensity <- future_lapply(1:nrow(data_combined()), function(x) mean(data_combined()[x,]))
      names(intensity) <- row.names(data_combined())
      intensity <- as.matrix(t(as.data.frame(intensity)))
      return(intensity)
    }else if(input$show_intensity == "1"){
      intensity <- future_lapply(1:nrow(data1()), function(x) mean(data1()[x,]))
      names(intensity) <- row.names(data1())
      intensity <- as.matrix(t(as.data.frame(intensity)))
      return(intensity)
    }else{
      intensity <- future_lapply(1:nrow(data2()), function(x) mean(data2()[x,]))
      names(intensity) <- row.names(data2())
      intensity <- as.matrix(t(as.data.frame(intensity)))
      return(intensity)
    }
  })
  
  # significance lables to determine colors on the plot
  mycolorg <- reactive({
    mycolor <- future_lapply(1:nrow(data), function(x,
                                             FC_CutOff = log2(input$fc_cut_off),
                                             P_cutOff = -log10(input$p_cut_off),
                                             curvature = input$curvature)
    {
      if(input$threshold == "line"){
        ifelse(log_p_as_y_axis()[x] < P_cutOff, "N.S.", ifelse(abs(log2FC()[x])<FC_CutOff, "N.S.", ifelse(log2FC()[x]>FC_CutOff, "S.Increased", "S.Decreased")))
      }else{
        ifelse(log_p_as_y_axis()[x] < (curvature/(abs(log2FC()[x])-FC_CutOff))+P_cutOff, "N.S.", ifelse(abs(log2FC()[x])<FC_CutOff, "N.S.", ifelse(log2FC()[x]>FC_CutOff, "S.Increased", "S.Decreased")))
      }
    })
    names(mycolor) <- row.names(data)
    mycolor <- as.data.frame(mycolor)
    mycolorg <- as.data.frame(t(mycolor))
    return(mycolorg)
  })
  
  # Prepare data for plot 
  dataplot <- reactive({
              if(input$show_intensity == "no"){
                dataplot <- as.data.frame(cbind(log2FC(), log_p_as_y_axis(), mycolorg()))
                names(dataplot) <- c("log2_FC", "log_p", "mycolorg")
                return(dataplot)
              }else{
                dataplot <- as.data.frame(cbind(log2FC(), log_p_as_y_axis(), mycolorg(), intensity()))
                names(dataplot) <- c("log2_FC", "log_p", "mycolorg", "intensity")
                return(dataplot)
              }
  })

  # For table output and for user download
  data_output <- reactive({
    if(input$p_adjust == "none"){
      data_output <- as.data.frame(cbind(2^log2FC(),log2FC(), 10^(-log_p_as_y_axis()), mycolorg()))
      names(data_output) <- c("Fold-change","Log2 fold-change", "p value", "Change")
      return(data_output)
    }else{
      data_output <- as.data.frame(cbind(2^log2FC(),log2FC(), 10^(-log_p_as_y_axis()), mycolorg()))
      names(data_output) <- c("Fold-change","Log2 fold-change", "adjusted p value", "Change")
      return(data_output)
    }
  })
  
  
  # Prepare plotting inputs
  # Custumized color selection
  colorpalette <- reactive({ c(input$color_picker_ns, input$color_picker_decrease, input$color_picker_increase) })
  
  # y axis limit
  max_log_p <- reactive({ max(dataplot()$log_p) })
  
  # Function curve
  
  fun1 <- reactive({
                    if(input$threshold == "curve"){
                      function(x,
                               FC_CutOff = log2(input$fc_cut_off),
                               P_cutOff = -log10(input$p_cut_off),
                               curvature = input$curvature)
                      {
                      ifelse(curvature/(abs(x)-FC_CutOff)>0, curvature/(abs(x)-FC_CutOff)+P_cutOff , NA)
                      }
                    }
  })
  
  # analysis status
  analysisstatus <- output$analysisstatus <- reactive({
    return(!is.null(dataplot()))
  })
  outputOptions(output, 'analysisstatus', suspendWhenHidden=FALSE)
  
  # render plots when analysis is done ---------------------------------------------
 
  
  output$volcano_plot_out <- renderPlotly({
    ggplotly(
      if(input$threshold == "line"){
        if(input$show_intensity == "no"){
        ggplot(dataplot(), aes(x=log2_FC, y=log_p)) +
          geom_point(aes(fill = factor(mycolorg), size = NULL),  color = "gray60", alpha = input$alpha_dots, shape = 21, stroke = 0.1) +
          xlab("Log2 fold-change") + ylab("-Log10 p-value") +
          geom_hline(yintercept = -log10(input$p_cut_off), color=input$color_picker_line, size=input$size_line, alpha = input$alpha_line, linetype = "dashed") +
          geom_vline(xintercept = log2(input$fc_cut_off), color=input$color_picker_line, size=input$size_line, alpha = input$alpha_line, linetype = "dashed") +
          geom_vline(xintercept = -log2(input$fc_cut_off),color=input$color_picker_line, size=input$size_line, alpha = input$alpha_line, linetype = "dashed") +
          scale_fill_manual(values=colorpalette()) +
          ylim(-0.2, max_log_p()) +
          theme_bw(base_size = 12) + labs(fill = 'Changes', color = 'Changes', size = "Intensity") + guides(size = FALSE, fill = guide_legend(reverse = TRUE))
        }else{
          ggplot(dataplot(), aes(x=log2_FC, y=log_p)) +
            geom_point(aes(fill = factor(mycolorg), size = intensity),  color = "gray60", alpha = input$alpha_dots, shape = 21, stroke = 0.1) +
            xlab("Log2 fold-change") + ylab("-Log10 p-value") +
            geom_hline(yintercept = -log10(input$p_cut_off), color=input$color_picker_line, size=input$size_line, alpha = input$alpha_line, linetype = "dashed") +
            geom_vline(xintercept = log2(input$fc_cut_off), color=input$color_picker_line, size=input$size_line, alpha = input$alpha_line, linetype = "dashed") +
            geom_vline(xintercept = -log2(input$fc_cut_off),color=input$color_picker_line, size=input$size_line, alpha = input$alpha_line, linetype = "dashed") +
            scale_fill_manual(values=colorpalette()) +
            ylim(-0.2, max_log_p()) +
            theme_bw(base_size = 12) + labs(fill = 'Changes', color = 'Changes', size = "Intensity") + guides(size = FALSE, fill = guide_legend(reverse = TRUE))
        }
      }else if(input$threshold == "curve"){
        if(input$show_intensity == "no"){
        ggplot(dataplot(), aes(x=log2_FC, y=log_p))+
          geom_point(aes(fill = factor(mycolorg), size = NULL),  color = "gray60", alpha = input$alpha_dots, shape = 21, stroke = 0.1) +
          xlab("Log2 fold-change") + ylab("-Log10 p-value") +
          stat_function(fun = fun1(), n=3000, geom = "line",color= input$color_picker_line, size=input$size_line, alpha = input$alpha_line, linetype = "dashed") + ylim(-0.2, max_log_p()) +
          scale_fill_manual(values=colorpalette()) +
          theme_bw(base_size = 12) + labs(fill = 'Changes', color = 'Changes', size = "Intensity") + guides(size = FALSE, fill = guide_legend(reverse = TRUE))
        }else{
          ggplot(dataplot(), aes(x=log2_FC, y=log_p))+
            geom_point(aes(fill = factor(mycolorg), size = intensity),  color = "gray60", alpha = input$alpha_dots, shape = 21, stroke = 0.1) +
            xlab("Log2 fold-change") + ylab("-Log10 p-value") +
            stat_function(fun = fun1(), n=3000, geom = "line",color=input$color_picker_line, size=input$size_line, alpha = input$alpha_line, linetype = "dashed") + ylim(-0.2, max_log_p()) +
            scale_fill_manual(values=colorpalette()) +
            theme_bw(base_size = 12) + labs(fill = 'Changes', color = 'Changes', size = "Intensity") + guides(size = FALSE, fill = guide_legend(reverse = TRUE))
        }
      }
    )
    })

  output$result_table <-   DT::renderDataTable(data_output(),
                                               filter = 'top',
                                               extensions = c('Scroller'),
                                               options = list(
                                                 autoWidth = TRUE,
                                                 pageLength = 50,
                                                 dom = 'Brtip',
                                                 scrollY = '500px',
                                                 scrollX = TRUE)
  )
  
  output$download_result_table <- downloadHandler(
    filename = function() {
      paste("result_table",Sys.time(),".csv", sep = "")
    },
    content = function(file) {
      write.csv(data_output(), file)
    }
  )
  
  
  # output$debug <- renderPrint({dataplot()})
   
}
