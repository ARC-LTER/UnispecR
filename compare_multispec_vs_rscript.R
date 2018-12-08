## Read in unispec data files <.spu> and plot
## AUTHOR: Ruby An
## DATE: 2018-06-24
## REVISED: 2018-11-29

# REQUIRED PACKAGES -------------------------------------------------------
require(tidyverse)
require(stringr)
require(lubridate)
# uses dplyr, tidyr, readr, ggplot2

## Useful vectors for standardizing names and filtering rows
WSG <- c("WSG1", "WSG2", "WSG3", "WSG23")
SHB <- c("SHB1", "SHB2")
site_list <- list("MAT", "LMAT", "MNAT", "NANT", "DHT", WSG, SHB, "HST")

CT <- c("CT","CT1","CT2")
NP <- c("F0.5","F1","F2","F5","F10","NP", "NO3", "NH4")
trtmt_list <- list(CT, "N", "P", NP)


# FUNCTIONS ---------------------------------------------------------------


calculate_ndvi_process <- function(tidydata, nir = c(820,890), red = c(640, 680)) {
  ## This is specific to this file, to compare processing types
  
  red_data <- tidydata %>% 
    filter(Wavelength >= red[1] & Wavelength <= red[2]) %>% 
    group_by(Site, Block, Treatment, Date, Measurement, Type, ProcessType) %>% 
    summarise(
      red = mean(Reflectance)
    )
  
  nir_data <- tidydata %>% 
    filter(Wavelength >= nir[1] & Wavelength <= nir[2]) %>% 
    group_by(Site, Block, Treatment, Date, Measurement,Type, ProcessType) %>% 
    summarise(
      nir = mean(Reflectance)
    )
  
  ndvi_data <- inner_join(nir_data, red_data) %>% 
    mutate(ndvi = (nir-red)/(red+nir)) 
  
  return(ndvi_data)
}

# LOAD DATA ---------------------------------------------------------------
load("multispec_data_2018.Rda")
load("rscript_data_2018.Rda")

df_multispec <- multispec_data_2018 %>% 
  mutate(ProcessType = "multispec") %>% 
  #select(Date, Site, Block, Treatment, Measurement, Wavelength, Reflectance, Type) %>% 
  filter(Wavelength >= 400 & Wavelength <= 1100)  %>% # Choose relevent wavelengths 
  mutate(Site = replace(Site, Site %in% WSG, "WSG")) %>% # tidy : combine WSG1 & WSG23
  mutate(Site = replace(Site, Site %in% SHB, "SHB"))   # tidy : combine SHB1 & SHB2 

df_types <- rscript_data_2018 %>% 
  mutate(ProcessType = "rscript") %>% 
  filter(Wavelength >= 400 & Wavelength <= 1100)  %>% # Choose relevent wavelengths 
  mutate(Site = replace(Site, Site %in% WSG, "WSG")) %>% # tidy : combine WSG1 & WSG23
  mutate(Site = replace(Site, Site %in% SHB, "SHB")) %>%  # tidy : combine SHB1 & SHB2 
  bind_rows(df_multispec)



## Save dataframe comparing processing types 
#save(df_types, file = "df_types.Rda")

# SPECTRA Comparison ---------------------------------------------------------

## PLOT SELECTION
### change the following vectors to explore all data 
### facet based on selection to explore across blocks, sites, etc. 
### default plot has dates on the x-axis
sites <-c("MAT") 
blocks <- c("B4") # c("B1", "B2", "B3", "B4")
trtmts <- c("CT2") # c(CT, NP, "N", "P"), careful of CT1 & CT2 sites
measures <- c("1", "2", "3", "4", "5")

## FILTER dataframe based on SELECTION
df_plot <- df_types %>% 
  filter(Site %in% sites) %>% 
  filter(Block %in% blocks) %>% 
  filter(Treatment %in% trtmts) %>% 
  filter(Measurement %in% measures) #%>% 
  #filter(str_detect(Type, "correct")) #%>% 
  #group_by(Date, Site, Block, Treatment, Measurement, Type, Wavelength) %>% 
  #summarize(n = n())

## PLOT SELECTED DATA 
ggplot(data = df_plot, mapping = aes(x = Wavelength, y = Reflectance, linetype=ProcessType)) +
  geom_line(aes(color=Type)) +
  facet_grid(Measurement ~ Date)



# NDVI Comparison ----------------------------------------------------------
### No evidence for difference between NDVI between processing types. 
### Lines for "multispec" vs "rscript" plot right over one another.
### Do actual stats on this? repeated measures ANOVA or dependent t-test? 

## PLOT SELECTION over DATES
sites <-c("MAT") #
blocks <- c("B1") # c("B1", "B2", "B3", "B4")
trtmts <- c("CT1") # c("CT", "NP", "N", "P")
measures <- c("1", "2", "3", "4", "5")

ndvi_types <- df_types %>% 
  filter(Site %in% sites) %>% 
  filter(Block %in% blocks) %>% 
  filter(Treatment %in% trtmts) %>% 
  calculate_ndvi_process()

ndvi_plot <- ndvi_types

ggplot(data = ndvi_plot, mapping = aes(x = Date, y = ndvi, color = Type, linetype = ProcessType)) +
  geom_point() +
  geom_line() + 
  facet_grid( Measurement ~ Type)


## PLOT SELECTION for Individual Date 
### change the following vectors to explore all data over dates
dates <- df_types %>% select(Date) %>% unique() %>% slice(1:n()) %>% c()
dates <- dates[[1]]
sites <-c("WSG") #
blocks <- c("B1") # c("B1", "B2", "B3", "B4")
trtmts <- c("CT1") # c("CT", "NP", "N", "P")
measures <- c("1", "2", "3", "4", "5")

ndvi_types <- df_types %>% 
  filter(Date %in% dates) %>% 
  filter(Site %in% sites) %>% 
  filter(Block %in% blocks) %>% 
  filter(Treatment %in% trtmts) %>% 
  calculate_ndvi_process()

ndvi_plot <- ndvi_types

ggplot(data = ndvi_plot, mapping = aes(x = ProcessType, y=ndvi, fill=Type)) +
  geom_bar(stat="identity") +
  facet_grid( Measurement ~ Type)


# STATS? ------------------------------------------------------------------


