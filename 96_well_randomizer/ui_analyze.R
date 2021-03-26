fluidPage(
  fluidRow(
    column(6,
           box(title = "View your experimental design (by Group)",
               status = "primary", 
               width =  12,
               solidHeader = TRUE,
               collapsible = FALSE,
               plotOutput("plate_user_uploaded")
           ),
           box(title = "Data uploaded:",
               status = "primary", 
               width =  12,
               solidHeader = TRUE,
               collapsible = FALSE,
               DATATABLE_Display_UI("data_table_display",boxwidth = 12,
                                    boxtitle = "Your experimental design")
          )
    ),
    column(6,
           box(title = "View randomized experimental design",
               status = "primary", 
               width =  12,
               solidHeader = TRUE,
               collapsible = FALSE,
               plotOutput("plate_randomized"),
               column(3),
               column(6, align="center",
               numericInput("seed_input", h4("Change a seed to re-randomize:"), 1, min = 1, max = 1000),
               verbatimTextOutput("value") # debug
               ),
               column(3)
           ),
           box(title = "Data randomized:",
               status = "primary", 
               width =  12,
               solidHeader = TRUE,
               collapsible = FALSE,
               DATATABLE_Display_UI("data_table_randomized_display",boxwidth = 12,
                                    boxtitle = "Randomized experimental design")
           )
    )
  )
)