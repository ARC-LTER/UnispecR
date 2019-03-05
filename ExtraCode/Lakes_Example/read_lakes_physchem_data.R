## Read-in Arctic LTER Lakes Data for Shiny app
## Purpose: Demonstration for Dan White
## Author: Ruby An
## Date: 2019-02-28 


# Required Packages  ------------------------------------------------------

library(tidyverse)
library(lubridate)

# Read in Data ------------------------------------------------------------

filename <- "Lakes_Example/ARC_Lakes_Physchem_1983-1989.csv"

physchem_data <- read_csv(file = filename, col_names = T, na = c(".", "NA"))


# Restructure Data --------------------------------------------------------

data <- physchem_data %>% 
  ## Rename columns (if necessary?)
  #rename(Sort_Chem = "SortChem_#", Rounded_Depth = "Rounded Depth (m)", Hydrolab_Depth = `Hydrolab Depth`)
  ## Convert Dates
  mutate(Date = lubridate::dmy(Date)) %>% 
  mutate(Year = as.integer(format(Date, format = "%Y"))) %>% 
  mutate(DOY = lubridate::yday(Date))# %>% 
  ## Gather Measurements 
  # gather(key = "Measure", value = "Value", IBVSvr4a_V:Secchi_m)

site_list <- unique(df$Site)
measurements <- names(df)[7:19]


# Plot Data ---------------------------------------------------------------
### Plot


which_measure <- "Cond_uS" 
sites <- c("Toolik",  "N3" )
years <- c(1983:1985)
start_date <- 150
end_date <- 200

sub_data <- data %>% 
  filter(Site %in% sites) %>% 
  filter(Year %in% years) %>% 
  filter(DOY >= start_date) %>% 
  filter(DOY <= end_date) 

data_to_plot <- sub_data  %>% 
  rename_at(vars(contains(which_measure)), funs(sub(which_measure, 'measure', .)))

ggplot(data = data_to_plot, mapping = aes(x = `Hydrolab Depth`, y = measure)) +
  geom_point(aes(color=DOY)) + 
  geom_path(aes(color=DOY)) +
  labs(x = "Depth (m)", 
       y = which_measure) +
  scale_x_reverse() + 
  coord_flip () + 
  facet_grid(Site ~ Year, scales = "free")


# Save data ---------------------------------------------------------------

savename <- "ARC_Lakes_Physchem_1983-1989"
write_rds(data, path = paste0(savename, ".rds"))

         