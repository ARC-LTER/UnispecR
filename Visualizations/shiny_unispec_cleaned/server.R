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

index_data <- read_rds("indices_2014-2019_tagged.rds") #load dataframe "index_data"


## Useful Objects for Plotting 
site_list <- c("HIST", "MAT", "LMAT", "MNAT", "NANT", "HTH", "WSG", "SHB")
block_list <- c("B1", "B2", "B3", "B4")

CT <- c("CT","CT1","CT2")
NP <- c("F10","NP")
NP_gradient <- c("F0.5","F1","F2","F5", "F10")
N_types <- c("NO3", "NH4")
trtmt_list <- list(CT, "N", "P", NP, NP_gradient, N_types)

## Formatting vectors 
np_colors <- RColorBrewer::brewer.pal(5, "YlGnBu")
np_colors[5] <- "green4"

### TEXT
Site.text <- c("MAT (1989)", "LMAT (2006)", "HIST (1981)", 
               "HTH (1989)", "MNAT (1997)", "NANT (1997)", 
               "SHB (1989)", "WSG (1989)")
names(Site.text) <- c("MAT", "LMAT", "HIST", "HTH", "MNAT", "NANT",
                      "SHB", "WSG")

## PLOT OPTIONS

# Color Palettes 
n_yellow <- rgb(255, 192, 0, maxColorValue = 255)
p_blue <- rgb(46, 117, 182, maxColorValue = 255)
np_green <- rgb(112, 173, 71, maxColorValue = 255) #lmat_colors[5] 
ct_gray <- rgb(175, 171, 171, maxColorValue = 255)

lmat_colors <- c(rgb(226, 240, 217, maxColorValue = 255), rgb(169, 209, 142, maxColorValue = 255), rgb(112, 173, 71, maxColorValue = 255), rgb(84, 130, 53, maxColorValue = 255), rgb(56, 87, 35, maxColorValue = 255))



# Define server logic required to draw plots
shinyServer(
  
  function(input, output, session) {
    
    
    # Reactive Conductor Functions  -------------------------------------------
    ## (see https://shiny.rstudio.com/articles/reactivity-overview.html)
    
    ctl_comp_select <- function(index_data) {
      sites <- unlist(site_list[as.numeric(input$ctl_comp_sites)])
      aggregate_ctls <- input$ctl_comp_aggregate == T
      
      sub_data <- index_data %>%
        filter(Site %in% sites) %>%
        filter(Treatment %in% CT) %>%
        mutate(Treatment = replace(Treatment, Treatment %in% CT & aggregate_ctls, "CT"), Treatment)  %>% #Choose index to graph
        filter(Year >= input$ctl_comp_years[1] & Year <= input$ctl_comp_years[2]) %>%
        mutate(Year = factor(Year)) %>%
        mutate(Block = factor(Block)) %>%
        # SUMMARIZE by block and site
        group_by(Year, DOY, Date, Site, Block, Treatment) %>%
        summarize_at(vars(NDVI:EVI2), mean, na.rm=T) %>%
        group_by(Year, DOY, Date, Site, Treatment) %>%
        group_by(N = n(), add = TRUE) %>% # add number of blocks per site to get Standard Error
        summarize_at(vars(NDVI:EVI2), funs(mean, sd), na.rm=T)
      
      return(sub_data)
    }
    
    bysite_select <- function(index_data) {
      sites <- unlist(site_list[as.numeric(input$bysite_sites)])
      trtmts <- unlist(trtmt_list[as.numeric(input$bysite_trtmts)])
      aggregate_ctls <- input$ctl_comp_aggregate == T
      
      
      sub_data <- index_data  %>% 
        filter(Site %in% sites) %>% 
        filter(Treatment %in% trtmts) %>% 
        mutate(Treatment = replace(Treatment, Treatment %in% CT & aggregate_ctls, "CT"), Treatment)  %>%
        filter(Year >= input$bysite_years[1] & Year <= input$bysite_years[2]) %>% 
        mutate(Year = factor(Year)) %>% 
        mutate(Block = factor(Block)) %>% 
        # SUMMARIZE by block and site
        group_by(Year, DOY, Date, Site, Block, Treatment) %>% 
        summarize_at(vars(NDVI:EVI2), mean, na.rm=T) %>% 
        group_by(Year, DOY, Date, Site, Treatment) %>% 
        group_by(N = n(), add = TRUE) %>% # add number of blocks per site to get Standard Error
        summarize_at(vars(NDVI:EVI2), funs(mean, sd), na.rm=T) 
      
      return(sub_data) 
    }

    
    byblock_select <- function(index_data) {
      sites <- input$byblock_site
      blocks <- input$byblock_blocks
      trtmts <- unlist(trtmt_list[as.numeric(input$byblock_trtmts)])
      aggregate_ctls <- input$ctl_comp_aggregate == T
      
      
      sub_data <- index_data  %>% 
        filter(Site %in% sites) %>% 
        filter(Treatment %in% trtmts) %>% 
        mutate(Treatment = replace(Treatment, Treatment %in% CT & aggregate_ctls, "CT"), Treatment)  %>%
        filter(Block %in% blocks) %>% 
        filter(Year >= input$byblock_years[1] & Year <= input$byblock_years[2]) %>% 
        mutate(Year = factor(Year)) %>% 
        mutate(Block = factor(Block)) %>% 
        # SUMMARIZE - by block
        group_by(Year, DOY, Date, Site, Block, Treatment) %>% 
        summarize_at(vars(NDVI:EVI2), funs(mean, sd), na.rm=T) %>% 
        group_by(N = n(), add = TRUE)
      
      return(sub_data)
    }
    
    byplot_select <- function(index_data) {
      sites <- input$byplot_site
      blocks <- input$byplot_blocks
      trtmts <- unlist(trtmt_list[as.numeric(input$byplot_trtmts)])
      measures <- input$byplot_measurement
      aggregate_ctls <- input$ctl_comp_aggregate == T
      
      
      # SELECTION - subset of full dataframe
      sub_data <- index_data  %>% 
        filter(Site %in% sites) %>% 
        filter(Treatment %in% trtmts) %>% 
        mutate(Treatment = replace(Treatment, Treatment %in% CT & aggregate_ctls, "CT"), Treatment)  %>%
        filter(Block %in% blocks) %>% 
        filter(Replicate %in% measures) %>% 
        filter(Year >= input$byplot_years[1] & Year <= input$byplot_years[2]) %>% 
        mutate(Year = factor(Year)) %>% 
        mutate(Block = factor(Block)) %>% 
        mutate(Replicate = factor(Replicate))  %>% 
        group_by(Year, DOY, Date, Site, Block,Treatment, Replicate) %>% 
        summarize_at(vars(NDVI:EVI2), funs(mean, sd), na.rm=T) 
      
      return(sub_data)
    }
    
    # Reactive Dataframes -----------------------------------------------------
    data <- reactiveValues(ctl_comp_sub_data = NULL,
                           bysite_sub_data = NULL,
                           byblock_sub_data = NULL,
                           byplot_sub_data = NULL)
    
    
    # Datatable Output --------------------------------------------------------
    
    output$ctl_comp_table <- DT::renderDataTable({
      DT::datatable(data$ctl_comp_sub_data, options = list(orderClasses = TRUE))
    })
    
    output$bysite_table <- DT::renderDataTable({
      DT::datatable(data$bysite_sub_data, options = list(orderClasses = TRUE))
    })
    
    output$byblock_table <- DT::renderDataTable({
      DT::datatable(data$byblock_sub_data, options = list(orderClasses = TRUE))
    })
    
    output$byplot_table <- DT::renderDataTable({
      DT::datatable(data$byplot_sub_data, options = list(orderClasses = TRUE))
    })
    
    # Plot Output -------------------------------------------------------------
    output$ctl_compPlot <- renderPlot({ ######## AGGREGRATE BY SITE
      sub_data <- ctl_comp_select(index_data)
      which_index <- input$ctl_comp_index
      
      ### Reactive Dataframe ###
      data$ctl_comp_sub_data <- sub_data %>% 
        select(Year, DOY, Date, Site, Treatment, contains(which_index))
      
      ### Plot
      index_tograph <- sub_data %>% #Choose index to graph
        rename_at(vars(contains(which_index)), funs(sub(which_index, 'index', .))) 
      
      ggplot(data = index_tograph, mapping = aes(x = DOY, y = index_mean, color=Site)) +
        geom_point() + 
        geom_line(aes(linetype=Treatment)) + 
        geom_errorbar(aes(ymin = index_mean - index_sd/sqrt(N) , ymax= index_mean + index_sd/sqrt(N))) + 
        labs(y = which_index) +
        facet_grid(. ~ Year) + 
        scale_color_manual(values = c("SHB" = "orange4", "HIST" = "darkgreen", "MAT" = "green4", "LMAT" = "chartreuse3",
                                      "WSG" = "dodgerblue2", "MNAT" = "darkorchid", "NANT" = "mediumpurple4", "HTH" = "firebrick"
                                      ))
      
    })
    
    output$sitePlot <- renderPlot({ ######## AGGREGRATE BY SITE
      sub_data <- bysite_select(index_data)
      which_index <- input$bysite_index
      
      ### Reactive Dataframe 
      data$bysite_sub_data <- sub_data %>% 
        select(Year, DOY, Date, Site, Treatment, contains(which_index))
      
      ### Plot
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
        theme_gray(base_size=18) +
        labs(y = which_index) + 
        facet_grid(Site ~ Year, labeller = labeller(Site = Site.text))
      
    })
    
    
    output$blockPlot <- renderPlot({ ######## AGGREGRATE BY BLOCK
      sub_data <- byblock_select(index_data)
      which_index <- input$byblock_index
      
      ### Reactive Dataframe
      data$byblock_sub_data <- sub_data %>% 
        select(Year, DOY, Date, Site, Block, Treatment, contains(which_index))
      
      ### Plot 
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
        theme_gray(base_size = 18) +
        labs(y = which_index) + 
        facet_grid(Block ~ Year) 
      
    })
    
    output$plotPlot <- renderPlot({ ######## AGGREGRATE BY BLOCK
      sub_data <- byplot_select(index_data)
      which_index <- input$byplot_index
      
      ### Reactive Dataframe
      
      data$byplot_sub_data <- sub_data %>% 
        select(Year, DOY, Date, Site, Block, Treatment, Replicate, contains(which_index), -contains("sd"))
      
      ### Plot
      index_tograph <- sub_data %>% #Choose index to graph
        rename_at(vars(contains(which_index)), funs(sub(which_index, 'index', .)))
      
      
      ggplot(data = index_tograph, mapping = aes(x = DOY, y = index_mean, color=Treatment)) +
        geom_point() + 
        geom_line(aes(linetype=Replicate)) + 
        scale_color_manual(values=c("CT" = "black", "CT1"="black", "CT2"="black",
                                    "N" = "blue2", "NO3" = "dodgerblue", "NH4" = "deepskyblue",
                                    "P" = "red2",
                                    "NP" = "green4",
                                    "F0.5" = np_colors[1],
                                    "F1" = np_colors[2],
                                    "F2" = np_colors[3],
                                    "F5" = np_colors[4],
                                    "F10" = np_colors[5]))  + 
        theme_gray(base_size = 18) +
        labs(y = which_index) + 
        facet_grid(Block ~ Year)
      
    })
    
    
    
  }
  
)