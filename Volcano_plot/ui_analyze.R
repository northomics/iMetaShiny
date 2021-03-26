
fluidRow( # this fluidrow is to ensure the extend the height
     box(width = 12,
         title = "Step 4. Analyze Differential Expressions",
         solidHeader = TRUE,
         status = "primary",
         volcano_plot_UI(id = "volcano_analysis")
     )
)
