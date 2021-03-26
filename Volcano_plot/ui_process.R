
conditionalPanel(
  condition = 'input.if_preprocess_data',
  box(title = "Step 3. Process data matrix before analysis",
      status = "primary", 
      width =  12,
      solidHeader = TRUE,
      collapsible = FALSE,
      datatableProcess_full_UI("data_table_preprocess")
  ),
  box(
    title = "Step 4. Go to analysis",
    width =  12,
    status = "primary",
    solidHeader = TRUE,
    collapsible = FALSE,
    actionButton("gotoanalysis2", 
                 icon = icon("arrow-right"),
                 label = "Go to differential expression analysis!",
                 style="float:left; color: #fff; background-color: #337ab7; border-color: #2e6da4"
    )
  )
)