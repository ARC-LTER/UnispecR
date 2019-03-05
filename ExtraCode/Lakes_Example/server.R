# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

## Required Packages & Data
library(tidyverse)
library(DT)
library(shiny)
library(markdown)


lake_data <- read_rds("ARC_Lakes_Physchem_1983-1989.rds") #load dataframe 

## Useful Objects for Plotting 
site_list <- unique(data$Site)
measure_list <- names(data)[7:19]

# Define server logic required to draw plots
shinyServer(
  
  function(input, output, session) {


# Reactive Conductor Functions  -------------------------------------------
    ## (see https://shiny.rstudio.com/articles/reactivity-overview.html)
    
    lakes_select <- function(lake_data) {
      sites <- unlist(site_list[as.numeric(input$lake_sites)])
      measures <- unlist(measure_list[as.numeric(input$lake_measures)]) 
      
      sub_data <- lake_data %>%
        filter(Site %in% sites) %>% 
        # filter(DOY >= input$start_date) %>% 
        # filter(DOY <= input$end_date) 
        filter(Year >= input$lake_years[1] & Year <= input$lake_years[2]) %>%
        mutate(Year = factor(Year)) 
      
      return(sub_data)
    }
    
# Reactive Dataframes -----------------------------------------------------
    data <- reactiveValues(lakes_sub_data = NULL)
    

# Datatable Output --------------------------------------------------------

    output$lakes_table <- DT::renderDataTable({
      DT::datatable(data$lakes_sub_data, options = list(orderClasses = TRUE))
    })
    
# Plot & Datatable Output -------------------------------------------------------------
    output$lakes_plot <- renderPlot({ ######## LAKES PHYSCHEM
      sub_data <- lakes_select(lake_data) 
      which_measure <- input$measurement
      
      ### Reactive Dataframe ###
      data$lakes_sub_data <- sub_data %>% 
        # pick measurement
        select(contains(which_measure), everything())
      
      ### Plot
      data_to_plot <- sub_data  %>% # Choose measurement to graph 
        rename_at(vars(contains(which_measure)), funs(sub(which_measure, 'measure', .)))

      ggplot(data = data_to_plot, mapping = aes(x = `Hydrolab Depth`, y = measure)) +
        geom_point(aes(color=DOY)) + 
        geom_path(aes(color=DOY)) +
        labs(x = "Depth (m)", 
             y = which_measure) +
        scale_x_reverse() + 
        coord_flip () + 
        facet_grid(Site ~ Year, scales = "free")
      
    })
    
  }
  
)
