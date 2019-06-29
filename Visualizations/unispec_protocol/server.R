#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

## Data 
index_data <- read_rds("2019_index_data.rds") %>% 
    unnest(Indices) %>% spread(Index, Value) %>% 
    mutate(DOY = lubridate::yday(DateTime))

## Vectors
WSG <- list("WSG1", "WSG23")
site_list <- list("HIST", "MAT", "LMAT", "MNAT", "NANT", "DHT", WSG, "SHB")
block_list <- c("B1", "B2", "B3", "B4")
CT <- c("CT","CT1","CT2")
NP <- c("F0.5","F1","F2","F5","F10","NP", "NO3", "NH4")
trtmt_list <- list(CT, "N", "P", NP)

## Plotting Formats
np_colors <- RColorBrewer::brewer.pal(5, "YlGnBu")


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    
    # Reactive Conductor Functions  -------------------------------------------
    ## (see https://shiny.rstudio.com/articles/reactivity-overview.html)
    bysite_select <- function(index_data) {
        sites <- unlist(site_list[as.numeric(input$bysite_sites)])
        blocks <- unlist(block_list[as.numeric(input$bysite_blocks)])
        trtmts <- unlist(trtmt_list[as.numeric(input$bysite_trtmts)])
        
        sub_data <- index_data  %>% 
            filter(Site %in% sites) %>% 
            filter(Block %in% blocks) %>% 
            filter(Treatment %in% trtmts) %>% 
            filter(DOY >= input$bysite_dates[1] & DOY <= input$bysite_dates[2]) %>% 
            mutate(Block = factor(Block)) %>% 
            # SUMMARIZE by block and site
            group_by(DOY, Date, Site, Block, Treatment) %>% 
            summarize_at(vars(NDVI:EVI2), mean, na.rm=T) %>% 
            group_by(DOY, Date, Site, Treatment) %>% 
            group_by(N = n(), add = TRUE) %>% # add number of blocks per site to get Standard Error
            summarize_at(vars(NDVI:EVI2), funs(mean, sd), na.rm=T) 
        
        return(sub_data) 
    }
    
    # Reactive Dataframes -----------------------------------------------------
    data <- reactiveValues(bysite_sub_data = NULL)
    
    # Datatable Output --------------------------------------------------------
    
    output$bysite_table <- DT::renderDataTable({
        DT::datatable(data$bysite_sub_data, options = list(orderClasses = TRUE))
    })


    # Plot Output -------------------------------------------------------------
    output$sitePlot <- renderPlot({ ######## AGGREGRATE BY SITE
        sub_data <- bysite_select(index_data)
        which_index <- input$bysite_index
        
        ### Reactive Dataframe 
        data$bysite_sub_data <- sub_data %>% 
            select(DOY, Date, Site, Treatment, contains(which_index))
        
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
            labs(y = which_index) + 
            facet_grid(Site ~ .) 
        
    })
})
