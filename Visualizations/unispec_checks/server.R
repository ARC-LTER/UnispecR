#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
# use the below options code if you wish to increase the file input limit, in this example file input limit is increased from 5MB to 9MB
# options(shiny.maxRequestSize = 9*1024^2)

shinyServer(function(input,output) {
    
    ## input$file is a data frame and contains the details around the name, size and temp location of the files uploaded
    # this reactive output display the content of the input$file dataframe
    output$filedf <- renderTable({
        if(is.null(input$file)){return ()}
        input$file # the file input data frame object that contains the file attributes
    })
    
    # Extract the file path for file
    output$filedf2 <- renderTable({
        if(is.null(input$file)){return ()}
        input$file$datapath # the file input data frame object that contains the file attributes
    })
    
    ## Below code to display the structure of the input file object
    output$fileob <- renderPrint({
        if(is.null(input$file)){return ()}
        str(input$file)
    })
    
    ## Side bar select input widget coming through renderUI()
    # Following code displays the select input widget with the list of file loaded by the user
    output$selectfile <- renderUI({
        if(is.null(input$file)) {return()}
        list(hr(), 
             helpText("Select the files for which you need to see data and summary stats"),
             selectInput("Select", "Select files", choices=input$file$name)
        )
        
    })
    
    
    ## Dataset code ##
    # This reactive output contains the dataset and display the dataset in table format
    output$table <- renderTable({ 
        if(is.null(input$file)){return()}
        read.table(file=input$file$datapath[input$file$name==input$Select], 
                   skip = 9, 
                   col.names = c("Wavelength", "ChB", "ChA"))
        
    })
    
    ## Metadata code ##
    # This reactive output contains the file metadata and displays the metadata in a text format
    output$metatable <- renderTable({ 
      if(is.null(input$file)){return()}
      read.table(file=input$file$datapath[input$file$name==input$Select], 
                 col.names = "Instrument_Metadata", # first 9 rows of .spu file are metadata
                 nrows=9) 
      
    })
    
    ## Summary Stats code ##
    # this reactive output contains the summary of the dataset and display the summary in table format
    output$summ <- renderPrint({
      if(is.null(input$file)){return()}
      str(read.table(file=input$file$datapath[input$file$name==input$Select], 
                         sep= ",", 
                         header = F, 
                         stringsAsFactors = F,
                         skip=9))})
    
    ## Plot Spectra code ##
    # This reacticev output contains the raw spectra in an x-y line plot format
    output$specplot <- renderPlot({
      if(is.null(input$file)){return()}
      
      ## read data
      read.table(file=input$file$datapath[input$file$name==input$Select], 
                       skip = 9, 
                       col.names = c("Wavelength", "ChB", "ChA")) %>% 
        
        ## tidy
        mutate(Reflectance = ChB/ChA) %>% 
        filter(Wavelength > 400, Wavelength < 1000) %>% 
        gather(key = Channel, value = Intensity, ChB, ChA) %>%
        gather(key = ref_part, value = Reflectance_Intensity, Intensity, Reflectance) %>% 
        
        ## viz
        ggplot(mapping = aes(x = Wavelength, y = Reflectance_Intensity)) +
          geom_line(aes(color=Channel)) +
          facet_wrap("ref_part", scales = "free")
      
    })
    
    
    ## MainPanel tabset renderUI code ##
    # the following renderUI is used to dynamically generate the tabsets when the file is loaded. 
    # Until the file is loaded, app will not show the tabset.
    output$tb <- renderUI({
        if(is.null(input$file)) {return()}
        else
            tabsetPanel(
                tabPanel("Input File Object DF ", tableOutput("filedf"), tableOutput("filedf2")),
                tabPanel("Input File Object Structure", verbatimTextOutput("fileob")),
                tabPanel("Dataset", tableOutput("metatable"), tableOutput("table")),
                tabPanel("Summary Stats", verbatimTextOutput("summ")),
                tabPanel("Spectra Plot", plotOutput("specplot")))
    })
})