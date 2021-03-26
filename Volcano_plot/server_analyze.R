#  __ data analysis------------------------------------------------------
observe({
  if(!is.null(rvalues$Data_processed) && !is.null(meta_table())){
    pg <- rvalues$Data_processed
    meta <- meta_table()
    callModule(volcano_plot_server, "volcano_analysis", data = pg, meta = meta)
  }
})