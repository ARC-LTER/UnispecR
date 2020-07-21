#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
shinyUI(fluidPage(
    
    titlePanel("Unispec QAQC - Upload multiple files"),
    
    sidebarLayout(
        sidebarPanel(
            fileInput("file","Upload .spu files", multiple = TRUE), # fileinput() function is used to get the file upload contorl option
            helpText("Default max. file size is 5MB"),
            helpText("The first 9 lines qq  of each file are instrument metadata."),
            
            checkboxInput(inputId = 'header', label = 'Header', value = TRUE),
            checkboxInput(inputId = "stringAsFactors", "stringAsFactors", FALSE),
            radioButtons(inputId = 'sep', label = 'Separator', choices = c(Comma=',',Semicolon=';',Tab='\t', Space=''), selected = ','),
            uiOutput("selectfile")
        ),
        
        
        mainPanel(
            uiOutput("tb")
            
        )
        
    )
))
