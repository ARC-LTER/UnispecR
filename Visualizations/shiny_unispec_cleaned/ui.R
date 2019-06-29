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


# Load Data ----------------------------------------------------------
index_data <- read_rds("indices_2014-2019.rds") #load dataframe "index_data"

# Useful Vectors ----------------------------------------------------------


site_list <- c("HIST", "MAT", "LMAT", "MNAT", "NANT", "HTH", "WSG", "SHB")
block_list <- c("1", "2", "3", "4")
index_list <- c("NDVI", "EVI", "EVI2")
year_list <- seq(min(index_data$Year), max(index_data$Year), by = 1) 

# Define UI for application----------------------------------------------------------
shinyUI(fluidPage(
  
  navbarPage("Arctic LTER Spectral Reflectance Data",
             
             tabPanel("Introduction",
                      
                      
                      column(width = 5, 
                             
                             includeMarkdown("introduction.md")
                             
                             ),
                      
                      column(6,
                        img(src='toolik_map.jpg', align = "center", 
                            height=700,
                            width=700)
                      )
                      
                      ),
             
             tabPanel("1. Control Plot Comparison",
                      
                      # SITE Plot Data ----
                      plotOutput('ctl_compPlot',
                                 click = "plot_click"),
                      #textOutput('vector'),
                      
                      hr(), # horizontal line break 
                      
                      # Fluid row layout with input and output definitions ----
                      fluidRow(
                        
                        column(width = 2, 
                               
                               # Input: ordinary selectize input without option groups
                               selectizeInput('ctl_comp_index', 
                                              h4('Vegetation Index'),
                                              choices = setNames(nm = index_list)),
                               
                               # Input: Specification of range within an interval ----
                               sliderInput("ctl_comp_years", 
                                           h4("Years"),
                                           min = min(year_list), max = max(year_list),
                                           value = c(min(year_list),max(year_list)),
                                           step=1,
                                           sep=""),
                               
                               # Input: Checkboxes for Site selection ----
                               checkboxGroupInput("ctl_comp_sites", 
                                                  h4("Vegetation Type Site (expmt. started)"), 
                                                  choices = list("HIST (1981)" = 1,
                                                                 "MAT (1989)"  = 2, 
                                                                 "LMAT (2006)"  = 3,
                                                                 "MNAT (1997)" = 4,
                                                                 "NANT (1997)" = 5,
                                                                 "HTH (1989)"  = 6,
                                                                 "WSG (1989)"  = 7,
                                                                 "SHB (1989)" = 8),
                                                  selected = c(1,2,3,4,5,6,7,8)),
                               
                               # Input: Radiobuttons to Aggregate CT Plots
                               radioButtons("ctl_comp_aggregate", label = h4("Aggregate CT plots"),
                                            choices = list("Aggregate" = T, "Disaggregate" = F), 
                                            selected = T)
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
                               
                               DT::dataTableOutput("ctl_comp_table")
                        )
                        
                        
                      )
             ),
             
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
                               sliderInput("bysite_years", 
                                           h4("Years"),
                                           min = min(year_list), max = max(year_list),
                                           value = c(min(year_list),max(year_list)),
                                           step=1,
                                           sep=""),
                               
                               p("\n Use the dashboard to select the subset of data to visualize in the plot (above) and dataframe (right).\n"),
                               p("The panels of the plot are faceted horizontally by Year and vertically by Site."),                               
                               p("Each panel in the plot shows the Vegetation Index plotted over the summer:
                                 the x-axis displays the Day of Year (DOY); the y-axis, the chosen vegetation index."),
                               p("The data is averaged by plot (5 measurements per plot) and then by block (1 to 4 blocks per site).
                                 The error bars indicate one standard deviation above and below.")   
                               
                              
                        ), 
                        
                        column(width = 1,
                               
                               # Input: Checkboxes for Site selection ----
                               checkboxGroupInput("bysite_sites", 
                                                  h4("Sites"), 
                                                  choices = list("HIST" = 1,
                                                                 "MAT"  = 2, 
                                                                 "LMAT"  = 3,
                                                                 "MNAT" = 4,
                                                                 "NANT" = 5,
                                                                 "HTH"  = 6,
                                                                 "WSG"  = 7,
                                                                 "SHB" = 8),
                                                  selected = 1)
                                              
                        ),
                        

                        
                        column(width = 1, 
                               
                               # Input: Checkboxes for Treatment  ----
                               checkboxGroupInput("bysite_trtmts", 
                                                  h4("Treatments"), 
                                                  choices = list("CT"  = 1,
                                                                 "N"   = 2,
                                                                 "P"   = 3,
                                                                 "NP"  = 4,
                                                                 "NP-gradient" = 5,
                                                                 "N-types" = 6
                                                                 # EXCT, EXNP{LF, SF, NF}, S, L to add 
                                                  ), 
                                                  selected = 1)
                        ),
                        
                        column(width = 4,
                               DT::dataTableOutput("bysite_table")
                               
                        )
                        
                      )
             ),
             
             tabPanel("3. Block-Level",
                      
                      # BLOCK Plot Data ----
                      
                      textOutput("byblock_vector"),
                      plotOutput('blockPlot'),
                      
                      hr(), # horizontal line break 
                      
                      # Fluid row layout with input and output definitions ----
                      fluidRow(
                        
                        column(2, 
                               
                               # Input: ordinary selectize input without option groups
                               selectizeInput('byblock_index', 
                                              h4('Index'),
                                              choices = setNames(nm = index_list)),
                               
                               
                               # an ordinary selectize input without option groups
                               selectizeInput('byblock_site', 
                                              h4('Site'),
                                              choices = setNames(nm = site_list)),
                               
                               # Input: Specification of range within an interval ----
                               sliderInput("byblock_years", 
                                           h4("Years"),
                                           min = min(year_list), max = max(year_list),
                                           value = c(min(year_list),max(year_list)),
                                           step=1,
                                           sep=""),

                               # Input: Checkboxes for Site selection ----
                               checkboxGroupInput("byblock_blocks", 
                                                  h4("Blocks"), 
                                                  choices = list("B1" = 1, 
                                                                 "B2" = 2,
                                                                 "B3" = 3,
                                                                 "B4" = 4),
                                                  selected = 1),
                        
                               # Input: Checkboxes for Treatment  ----
                               checkboxGroupInput("byblock_trtmts", 
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
                               p("The panels of the plot are faceted horizontally by Year and vertically by Block."),                               
                               p("Each panel in the plot shows the Vegetation Index plotted over the summer:
                                 the x-axis displays the Day of Year (DOY); the y-axis, the chosen vegetation index."),
                               p("The data is averaged by plot (5 measurements per plot).
                                 The error bars indicate one standard deviation above and below.")                               
                        ),
                        
                        column(width = 4, 
                               
                               DT::dataTableOutput("byblock_table")
                        )
                      )
             ),
             
             tabPanel("4. Plot-Level",
                      
                      # PLOT Plot Data ----
                      textOutput("plot"),
                      plotOutput('plotPlot'),
                      
                      hr(), # horizontal line break 
                      
                      # Fluid row layout with input and output definitions ----
                      fluidRow(
                        
                        
                        
                        column(2, 
                               
                               # Input: ordinary selectize input without option groups
                               selectizeInput('byplot_index', 
                                              h4('Index'),
                                              choices = setNames(nm = index_list)),
                               
                               
                               # an ordinary selectize input without option groups
                               selectizeInput('byplot_site', 
                                              h4('Site'),
                                              choices = setNames(nm = site_list)),
                               
                               
                               # Input: Specification of range within an interval ----
                               sliderInput("byplot_years", 
                                           h4("Years"),
                                           min = min(year_list), max = max(year_list),
                                           value = c(min(year_list),max(year_list)),
                                           step=1,
                                           sep=""),

                               
                               # Input: Checkboxes for Site selection ----
                               checkboxGroupInput("byplot_blocks", 
                                                  h4("Blocks"), 
                                                  choices = list("B1" = 1, 
                                                                 "B2" = 2,
                                                                 "B3" = 3,
                                                                 "B4" = 4),
                                                  selected = 1),
                               
                               # Input: Checkboxes for Measurement ----
                               checkboxGroupInput("byplot_measurement", 
                                                  h4("Measurements"), 
                                                  choices = list("1" = 1, 
                                                                 "2" = 2,
                                                                 "3" = 3,
                                                                 "4" = 4,
                                                                 "5" = 5),
                                                  selected = 1),

                               # Input: Checkboxes for Treatment  ----
                               checkboxGroupInput("byplot_trtmts", 
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
                               p("The panels of the plot are faceted horizontally by Year and vertically by Block."),                               
                               p("Each panel in the plot shows the Vegetation Index plotted over the summer:
                                 the x-axis displays the Day of Year (DOY); the y-axis, the chosen vegetation index."),
                               p("Each line in a panel represents 1 measurement location in a plot.")                               
                        ),
                        
                        column(width = 4, 
                               
                               DT::dataTableOutput("byplot_table")
                        )
                        
                      )
             )
  )
  
))
