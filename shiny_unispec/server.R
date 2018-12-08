#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(tidyverse)
library(shiny)
library(markdown)

load("unispec_indices_summary_dataframe.Rda") #load dataframe "index_data"

site_list <- c("HST", "MAT", "LOF", "MNAT", "NANT", "DHT", "WSG", "SHB")
block_list <- c("B1", "B2", "B3", "B4")

CT <- c("CT","CT1","CT2")
NP <- c("F0.5","F1","F2","F5","F10","NP", "NO3", "NH4")
trtmt_list <- list(CT, "N", "P", NP)


# Plotting Vectors --------------------------------------------------------
np_colors <- RColorBrewer::brewer.pal(5, "YlGnBu")

# Define server logic required to draw plots
shinyServer(
  
  function(input, output, session) {
    
    output$siteCompPlot <- renderPlot({ ######## AGGREGRATE BY SITE
      sites <- unlist(site_list[as.numeric(input$bysiteComp_sites)])
      
      # SELECTION - subset of full dataframe
      sub_data <- index_data  %>% 
        filter(Site %in% sites) %>% 
        filter(Treatment %in% CT) %>% 
        filter(Year >= input$bysiteComp_years[1] & Year <= input$bysiteComp_years[2]) %>% 
        mutate(Year = factor(Year)) %>% 
        mutate(Block = factor(Block)) %>% 
        # SUMMARIZE by block and site
        group_by(Year, DOY, Date, Site, Block, Treatment) %>% 
        summarize_at(vars(NDVI:LAI), mean, na.rm=T) %>% 
        group_by(Year, DOY, Date, Site, Treatment) %>% 
        group_by(N = n(), add = TRUE) %>% # add number of blocks per site to get Standard Error
        summarize_at(vars(NDVI:LAI), funs(mean, sd), na.rm=T) 
      
      which_index <- input$bysiteComp_index
      index_tograph <- sub_data %>% #Choose index to graph
        rename_at(vars(contains(which_index)), funs(sub(which_index, 'index', .))) 
      
      ggplot(data = index_tograph, mapping = aes(x = DOY, y = index_mean, color=Site)) +
        geom_point() + 
        geom_line(aes(linetype=Treatment)) + 
        geom_errorbar(aes(ymin = index_mean - index_sd/sqrt(N) , ymax= index_mean + index_sd/sqrt(N))) + 
        labs(y = which_index) +
        facet_grid(. ~ Year)
      
      
      
    })
    
    output$sitePlot <- renderPlot({ ######## AGGREGRATE BY SITE
      sites <- unlist(site_list[as.numeric(input$bysite_sites)])
      trtmts <- unlist(trtmt_list[as.numeric(input$bysite_trtmts)])
      
      
      # SELECTION - subset of full dataframe
      sub_data <- index_data  %>% 
        filter(Site %in% sites) %>% 
        filter(Treatment %in% trtmts) %>% 
        filter(Year >= input$bysite_years[1] & Year <= input$bysite_years[2]) %>% 
        mutate(Year = factor(Year)) %>% 
        mutate(Block = factor(Block)) %>% 
        # SUMMARIZE by block and site
        group_by(Year, DOY, Date, Site, Block, Treatment) %>% 
        summarize_at(vars(NDVI:LAI), mean, na.rm=T) %>% 
        group_by(Year, DOY, Date, Site, Treatment) %>% 
        group_by(N = n(), add = TRUE) %>% # add number of blocks per site to get Standard Error
        summarize_at(vars(NDVI:LAI), funs(mean, sd), na.rm=T) 
      
      which_index <- input$bysite_index
      index_tograph <- sub_data %>% #Choose index to graph
        rename_at(vars(contains(which_index)), funs(sub(which_index, 'index', .)))
      
      
      ggplot(data = index_tograph, mapping = aes(x = DOY, y = index_mean, color=Treatment)) +
        geom_point() + 
        geom_line() + 
        geom_errorbar(aes(ymin = index_mean - index_sd/sqrt(N) , ymax= index_mean + index_sd/sqrt(N))) + 
        scale_color_manual(values=c("CT" = "black", "CT1"="black", "CT2"="black",
                                    "N" = "blue2", "NO3" = "dodgerblue", "NH4" = "deepskyblue",
                                    "P" = "red2",
                                    "NP" = "green4",
                                    "F0.5" = np_colors[1],
                                    "F1" = np_colors[2],
                                    "F2" = np_colors[3],
                                    "F5" = np_colors[4],
                                    "F10" = np_colors[5]))  + 
        labs(y = which_index) + 
        facet_grid(Site ~ Year) 
      
      
    })
    
    
    output$blockPlot <- renderPlot({ ######## AGGREGRATE BY BLOCK
      sites <- input$byblock_site
      blocks <- input$byblock_blocks
      trtmts <- unlist(trtmt_list[as.numeric(input$byblock_trtmts)])
      
      
      # SELECTION - subset of full dataframe
      sub_data <- index_data  %>% 
        filter(Site %in% sites) %>% 
        filter(Treatment %in% trtmts) %>% 
        filter(Block %in% blocks) %>% 
        filter(Year >= input$byblock_years[1] & Year <= input$byblock_years[2]) %>% 
        mutate(Year = factor(Year)) %>% 
        mutate(Block = factor(Block)) %>% 
        # SUMMARIZE - by block
        group_by(Year, DOY, Date, Site, Block, Treatment) %>% 
        summarize_at(vars(NDVI:LAI), funs(mean, sd), na.rm=T) %>% 
        group_by(N = n(), add = TRUE)
      
        which_index <- input$byblock_index
        index_tograph <- sub_data %>% #Choose index to graph
          rename_at(vars(contains(which_index)), funs(sub(which_index, 'index', .)))
        
        
        ggplot(data = index_tograph, mapping = aes(x = DOY, y = index_mean, color=Treatment)) +
          geom_point() + 
          geom_line() + 
          geom_errorbar(aes(ymin = index_mean - index_sd/sqrt(N) , ymax= index_mean + index_sd/sqrt(N))) + 
          scale_color_manual(values=c("CT" = "black", "CT1"="black", "CT2"="black",
                                      "N" = "blue2", "NO3" = "dodgerblue", "NH4" = "deepskyblue",
                                      "P" = "red2",
                                      "NP" = "green4",
                                      "F0.5" = np_colors[1],
                                      "F1" = np_colors[2],
                                      "F2" = np_colors[3],
                                      "F5" = np_colors[4],
                                      "F10" = np_colors[5]))  + 
          labs(y = which_index) + 
          facet_grid(Block ~ Year) 

    })
    
    output$plotPlot <- renderPlot({ ######## AGGREGRATE BY BLOCK
      sites <- input$byplot_site
      blocks <- input$byplot_blocks
      trtmts <- unlist(trtmt_list[as.numeric(input$byplot_trtmts)])
      measures <- input$byplot_measurement
      
      
      # SELECTION - subset of full dataframe
      sub_data <- index_data  %>% 
        filter(Site %in% sites) %>% 
        filter(Treatment %in% trtmts) %>% 
        filter(Block %in% blocks) %>% 
        filter(Measurement %in% measures) %>% 
        filter(Year >= input$byplot_years[1] & Year <= input$byplot_years[2]) %>% 
        mutate(Year = factor(Year)) %>% 
        mutate(Block = factor(Block)) %>% 
        mutate(Measurement = factor(Measurement))  %>% 
        group_by(Year, DOY, Date, Site, Block,Treatment, Measurement) %>% 
        summarize_at(vars(NDVI:LAI), funs(mean, sd), na.rm=T) 

    
      which_index <- input$byplot_index
      index_tograph <- sub_data %>% #Choose index to graph
        rename_at(vars(contains(which_index)), funs(sub(which_index, 'index', .)))
      
      
      ggplot(data = index_tograph, mapping = aes(x = DOY, y = index_mean, color=Treatment)) +
        geom_point() + 
        geom_line(aes(linetype=Measurement)) + 
        scale_color_manual(values=c("CT" = "black", "CT1"="black", "CT2"="black",
                                    "N" = "blue2", "NO3" = "dodgerblue", "NH4" = "deepskyblue",
                                    "P" = "red2",
                                    "NP" = "green4",
                                    "F0.5" = np_colors[1],
                                    "F1" = np_colors[2],
                                    "F2" = np_colors[3],
                                    "F5" = np_colors[4],
                                    "F10" = np_colors[5]))  + 
        labs(y = which_index) + 
        facet_grid(Block ~ Year)
      
      
    })
    
  }
  
)
