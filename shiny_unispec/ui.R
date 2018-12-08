#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(markdown)


# Useful Vectors ----------------------------------------------------------
load("unispec_indices_summary_dataframe.Rda") #load dataframe "index_data"

site_list <- c("HST", "MAT", "LOF", "MNAT", "NANT", "DHT", "WSG", "SHB")
block_list <- c("1", "2", "3", "4")
index_list <- c("NDVI", "EVI", "EVI2", "WBI", "PRI_550", "PRI_570", "WBI", "Chl", "LAI")
max_year <- 2018
year_list <- seq(2007, max_year, by = 1) 
# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  navbarPage("Aggregation Level",
             tabPanel("Site Comparison",
                      
                      # SITE Plot Data ----
                      plotOutput('siteCompPlot'),
                      #textOutput('vector'),
                      
                      hr(), # horizontal line break 
                      
                      # Fluid row layout with input and output definitions ----
                      fluidRow(
                        
                        column(3, 
                               
                               # Input: ordinary selectize input without option groups
                               selectizeInput('bysiteComp_index', 'Index', choices = setNames(nm = index_list)),
                               
                               # Input: Specification of range within an interval ----
                               sliderInput("bysiteComp_years", "Years:",
                                           min = min(year_list), max = max(year_list),
                                           value = c(min(year_list),max(year_list)),
                                           step=1,
                                           sep="")
                        ),
                        
                        column(4, offset=1,
                               
                               # Input: Checkboxes for Site selection ----
                               checkboxGroupInput("bysiteComp_sites", 
                                                  h3("Sites - Veg Type"), 
                                                  choices = list("HST" = 1,
                                                                 "MAT"  = 2, 
                                                                 "LOF"  = 3,
                                                                 "MNAT" = 4,
                                                                 "NANT" = 5,
                                                                 "DHT"  = 6,
                                                                 "WSG"  = 7,
                                                                 "SHB" = 8),
                                                  selected = 1)
                        )
                        
                      )
             ),
             
             tabPanel("Site",
                      
                      # SITE Plot Data ----
                      plotOutput('sitePlot'),
                      #textOutput('vector'),
                      
                      hr(), # horizontal line break 
                      
                      # Fluid row layout with input and output definitions ----
                      fluidRow(
                        
                        column(3, 
                               
                               # Input: ordinary selectize input without option groups
                               selectizeInput('bysite_index', 'Index', choices = setNames(nm = index_list)),
                               
                               # Input: Specification of range within an interval ----
                               sliderInput("bysite_years", "Years:",
                                           min = min(year_list), max = max(year_list),
                                           value = c(min(year_list),max(year_list)),
                                           step=1,
                                           sep="")
                        ),
                        
                        column(4, offset=1,
                               
                               # Input: Checkboxes for Site selection ----
                               checkboxGroupInput("bysite_sites", 
                                                  h3("Sites - Veg Type"), 
                                                  choices = list("HST" = 1,
                                                                 "MAT"  = 2, 
                                                                 "LOF"  = 3,
                                                                 "MNAT" = 4,
                                                                 "NANT" = 5,
                                                                 "DHT"  = 6,
                                                                 "WSG"  = 7,
                                                                 "SHB" = 8),
                                                  selected = 1)
                        ),
                        
                        column(4, 
                               # Input: Checkboxes for Treatment  ----
                               checkboxGroupInput("bysite_trtmts", 
                                                  h3("Treatments"), 
                                                  choices = list("CT"  = 1,
                                                                 "N"   = 2,
                                                                 "P"   = 3,
                                                                 "NP"  = 4
                                                                 # EXCT, EXNP{LF, SF, NF}, S, L to add 
                                                  ), 
                                                  selected = 1)
                        )
                        
                      )
             ),
             
             tabPanel("Block",
                      
                      # BLOCK Plot Data ----
                      
                      textOutput("byblock_vector"),
                      plotOutput('blockPlot'),
                      
                      hr(), # horizontal line break 
                      
                      # Fluid row layout with input and output definitions ----
                      fluidRow(
                        
                        
                        
                        column(3, 
                               
                               # Input: ordinary selectize input without option groups
                               selectizeInput('byblock_index', 'Index', choices = setNames(nm = index_list)),
                               
                               
                               # an ordinary selectize input without option groups
                               selectizeInput('byblock_site', 'Site', choices = setNames(nm = site_list)),
                               
                               # Input: Specification of range within an interval ----
                               sliderInput("byblock_years", "Years:",
                                           min = min(year_list), max = max(year_list),
                                           value = c(min(year_list),max(year_list)),
                                           step=1,
                                           sep="")
                        ),
                        
                        column(4, offset=1,
                               
                               # Input: Checkboxes for Site selection ----
                               checkboxGroupInput("byblock_blocks", 
                                                  h3("Blocks"), 
                                                  choices = list("B1" = 1, 
                                                                 "B2" = 2,
                                                                 "B3" = 3,
                                                                 "B4" = 4),
                                                  selected = 1)
                        ),
                        
                        column(4, 
                               # Input: Checkboxes for Treatment  ----
                               checkboxGroupInput("byblock_trtmts", 
                                                  h3("Treatments"), 
                                                  choices = list("CT"  = 1,
                                                                 "N"   = 2,
                                                                 "P"   = 3,
                                                                 "NP"  = 4
                                                                 # EXCT, EXNP{LF, SF, NF}, S, L to add 
                                                  ), 
                                                  selected = 1)
                        )
                        
                      )
             ),
             
             tabPanel("Plot",
                      
                      # PLOT Plot Data ----
                      textOutput("plot"),
                      plotOutput('plotPlot'),
                      
                      hr(), # horizontal line break 
                      
                      # Fluid row layout with input and output definitions ----
                      fluidRow(
                        
                        
                        
                        column(3, 
                               
                               # Input: ordinary selectize input without option groups
                               selectizeInput('byplot_index', 'Index', choices = setNames(nm = index_list)),
                               
                               
                               # an ordinary selectize input without option groups
                               selectizeInput('byplot_site', 'Site', choices = setNames(nm = site_list)),
                               
                               
                               # Input: Specification of range within an interval ----
                               sliderInput("byplot_years", "Years:",
                                           min = min(year_list), max = max(year_list),
                                           value = c(min(year_list),max(year_list)),
                                           step=1,
                                           sep="")
                        ),
                        
                        column(4, offset=1,
                               
                               # Input: Checkboxes for Site selection ----
                               checkboxGroupInput("byplot_blocks", 
                                                  h3("Blocks"), 
                                                  choices = list("B1" = 1, 
                                                                 "B2" = 2,
                                                                 "B3" = 3,
                                                                 "B4" = 4),
                                                  selected = 1),
                               
                               # Input: Checkboxes for Measurement ----
                               checkboxGroupInput("byplot_measurement", 
                                                  h3("Measurements"), 
                                                  choices = list("1" = 1, 
                                                                 "2" = 2,
                                                                 "3" = 3,
                                                                 "4" = 4,
                                                                 "5" = 5),
                                                  selected = 1)
                        ),
                        
                        column(4, 
                               # Input: Checkboxes for Treatment  ----
                               checkboxGroupInput("byplot_trtmts", 
                                                  h3("Treatments"), 
                                                  choices = list("CT"  = 1,
                                                                 "N"   = 2,
                                                                 "P"   = 3,
                                                                 "NP"  = 4
                                                                 # EXCT, EXNP{LF, SF, NF}, S, L to add 
                                                  ), 
                                                  selected = 1)
                        )
                        
                      )
             )
  )
  
))
