#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)
library(tidyverse)
library(markdown)


# Useful Vectors ----------------------------------------------------------
data <- read_rds("ARC_Lakes_Physchem_1983-1989.rds") #load dataframe 

## Useful Objects for Plotting 
site_list <- unique(data$Site)
year_list <- seq(min(data$Year), max(data$Year), by = 1) 
measure_list <- names(data)[7:19]


# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  navbarPage("Arctic LTER Lakes Physchem Data",
             
             
             tabPanel("Physical Chemistry",
                      
                      # LAKE Plot Data ----
                      plotOutput('lakes_plot', height = "500px",
                                 click = "plot_click"),
                      #textOutput('vector'),
                      
                      hr(), # horizontal line break 
                      
                      # Fluid row layout with input and output definitions ----
                      fluidRow(
                        
                        column(width = 2, 
                               
                               # Input: ordinary selectize input without option groups
                               selectizeInput('measurement', 
                                              h4('Measurement Type'),
                                              choices = setNames(nm = measure_list)),
                               
                               # Input: Specification of range within an interval ----
                               sliderInput("lake_years", 
                                           h4("Years"),
                                           min = min(year_list), max = max(year_list),
                                           value = c(min(year_list),max(year_list)),
                                           step=1,
                                           sep=""),
                               
                               # Input: Checkboxes for Site selection ----
                               checkboxGroupInput("lake_sites", 
                                                  h4("Sites"), 
                                                  choices = list("Toolik" = 1,
                                                                 "N3"  = 2, 
                                                                 "N2"  = 3,
                                                                 "N2 Ref" = 4,
                                                                 "N2 Fert" = 5,
                                                                 "N1"  = 6,
                                                                 "Toolik Limno Bay"  = 7),
                                                  selected = 1)
                        ),
                        
                        column(width = 3,
                               
                               p("some kind of useful explanation here.")
                               
                        ),
                        
                        column(width = 4, 
                               
                               DT::dataTableOutput("lakes_table")
                        )
                        
                        
                      )
             )
  )
  
))
