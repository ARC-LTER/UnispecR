#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(tidyverse)
library(shiny)

index_data <- read_rds("2019_index_data.rds") %>% 
    unnest(Indices) %>% spread(Index, Value) %>% 
    mutate(DOY = lubridate::yday(DateTime))


site_list <- c("HIST", "MAT", "LMAT", "MNAT", "NANT", "DHT", "WSG", "SHB")
block_list <- c("1", "2", "3", "4")
index_list <- c("NDVI", "EVI", "EVI2")
year_list <- seq(min(lubridate::year(index_data$DateTime)), max((lubridate::year(index_data$DateTime))), by = 1) 
date_list <- unique(lubridate::yday(index_data$DateTime))

# Define UI for application 
shinyUI(fluidPage(
    
    navbarPage("Arctic LTER Spectral Reflectance Data",
               
               tabPanel("2. Site-Level",
                        
                        # SITE Plot Data ----
                        plotOutput('sitePlot', width = "100%", height = "700px"),
                        #textOutput('vector'),
                        
                        hr(), # horizontal line break 
                        
                        # Fluid row layout with input and output definitions ----
                        fluidRow(
                            
                            column(2, 
                                   
                                   # Input: ordinary selectize input without option groups
                                   selectizeInput('bysite_index', 
                                                  h4('Vegetation Index'),
                                                  choices = setNames(nm = index_list)),
                                   
                                   # Input: Specification of range within an interval ----
                                   sliderInput("bysite_dates", 
                                               h4("Dates"),
                                               min = min(date_list), max = max(date_list),
                                               value = c(min(date_list),max(date_list)),dragRange = T,
                                               step=1,
                                               sep=""),
                                   
                                   # Input: Checkboxes for Site selection ----
                                   checkboxGroupInput("bysite_sites", 
                                                      h4("Sites"), 
                                                      choices = list("HIST" = 1,
                                                                     "MAT"  = 2, 
                                                                     "LMAT"  = 3,
                                                                     "MNAT" = 4,
                                                                     "NANT" = 5,
                                                                     "DHT"  = 6,
                                                                     "WSG"  = 7,
                                                                     "SHB" = 8),
                                                      selected = 1),
                                   
                                   # Input: Checkboxes for Blocks  ----
                                   checkboxGroupInput("bysite_blocks", 
                                                      h4("Blocks"), 
                                                      choices = list("B1" = 1, 
                                                                     "B2" = 2,
                                                                     "B3" = 3,
                                                                     "B4" = 4),
                                                      selected = 1)),
                                   
                                   # Input: Checkboxes for Treatment  ----
                                   checkboxGroupInput("bysite_trtmts", 
                                                      h4("Treatments"), 
                                                      choices = list("CT"  = 1,
                                                                     "N"   = 2,
                                                                     "P"   = 3,
                                                                     "NP"  = 4
                                                                     # EXCT, EXNP{LF, SF, NF}, S, L to add 
                                                      ), 
                                                      selected = 1)
                            ), 
                            
                            column(width = 3,
                                   
                                   p("Use the dashboard on the left to select the subset of data to visualize in the plot (above) and dataframe (right)."),
                                   p("The panels of the plot are faceted horizontally by Year and vertically by Site."),                               
                                   p("Each panel in the plot shows the Vegetation Index plotted over the summer:
                                 the x-axis displays the Day of Year (DOY); the y-axis, the chosen vegetation index."),
                                   p("The data is averaged by plot (5 measurements per plot) and then by block (1 to 4 blocks per site).
                                 The error bars indicate one standard deviation above and below.")                               
                            ),
                            
                            column(width = 4, 
                                   
                                   DT::dataTableOutput("bysite_table")
                            )
                            
                        )
               )
        
    )
)
