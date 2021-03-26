fluidPage(
box(title = "boxtitle",
    status = "primary", 
    width =  12,
    solidHeader = TRUE,
    collapsible = TRUE,
    fluidRow(
      column(12,
             fileInput('file1', 'Upload the proteinGroups.txt file here: ',
                       accept = c(
                         'text/csv',
                         'text/comma-separated-values',
                         'text/tab-separated-values',
                         'text/plain',
                         '.csv',
                         '.tsv'
                       ),
                       placeholder = "proteinGroups.txt"
             ),
             h4("Options for cleaning proteinGroups data:"),
             checkboxInput('Potential_contaminant', 'Remove potential contaminant?', TRUE),
             checkboxInput('Reverse_sequence', 'Remove reverse sequence?', TRUE),
             checkboxInput('Only_identified_by_site', 'Remove only identified by site?', TRUE),
             selectInput('numeric_data_source', "Select data:", choices = c("LFQ intensity" = "lfq_intensity", "Intensity" = "intensity"), selected = "LFQ intensity", multiple = FALSE)
      )
    )
),
data_grouping_sets_UI("group_settings",
                      boxwidth = 4,
                      boxtitle = "Group settings")
)