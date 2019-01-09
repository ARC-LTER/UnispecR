## SCrap code for shiny app:

## Interactive Plot Clicking code

### server part 
output$click_notes <- renderPrint({
  cat("input$plot_click:\n")
  nearPoints(data$ctl_comp_sub_data, input$plot_click, addDist = F)
})

### ui part
verbatimTextOutput("click_notes")