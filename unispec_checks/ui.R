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
            fileInput("key_file","Upload Field Key files", multiple = TRUE), 
            
            uiOutput("selectfile")
        ),
        
        
        mainPanel(
            uiOutput("tb")
            
        )
        
    )
))
