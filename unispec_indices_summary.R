### Reads in LTER NDVI data from 2007-2016, reads in LTER data from 2017 & 2018
## Author: Ruby
## Date: July 2018
## Revised: November 2018


# Spectral Band Definitions -----------------------------------------------

# Lorna reflectance	570-680
# Lorna reflectance	725-1000
# Red defined by ITEX	560-680
# NIR defined by ITEX 	725-1000
# Blue defined by MODIS	459-479
# Red defined by MODIS	620-670
# NIR defined by MODIS	841-876
# Blue defined by SKYE	455-480
# Red defined by SKYE	620-680
# NIR defined by SKYE	830-880


# Red defined by GIS-drone (2018) 640-680
# NIR defined by GIS-drone (2018) 820-890

# Vegetation Indices Equations
# 
# NDIV = (NIR-Red)/(NIR+Red)
# 
# EVI = 2.5*((NIR-Red)/(NIR+6*Red-7.5*Blue+1))
# 
# EVI2 = 2.5*((NIR-Red)/(NIR+2.4*Red+1))
# 
# PRI (550 reference) = (550nm-531nm)/(550nm+531nm)
# 
# PRI (570 reference) = (570nm-531nm)/(570nm+531nm)
# 
# WBI = 900nm/970nm
# 
# Chl Index = (750nm-705nm)/(750nm+705nm)



# REQUIRED PACKAGES -------------------------------------------------------
require(tidyverse)
require(stringr)
require(lubridate)


# FUNCTIONS ---------------------------------------------------------------


calculate_ndvi <- function(tidydata,  band_defn = "MODIS") {
  if (band_defn == "ITEX") {
    red <- c(560, 680)
    nir <- c(725, 1000)
  } else if (band_defn == "MODIS") {
    
    red <- c(620, 670)
    nir <- c(841, 876)
    blue <- c(459, 479)
  } else if (band_defn == "SKYE" ) {
    red <- c(620, 680)
    nir <- c(830, 880)
    blue <- c(455, 480)
  } else {
    print("ERROR - specify band definition")
  }
  
  # Default MODIS bands for Red & NIR 
  red_data <- tidydata %>% 
    filter(Wavelength >= red[1] & Wavelength <= red[2]) %>% 
    group_by(Site, Block, Treatment, Date, Measurement) %>% 
    summarise(
      red = mean(Reflectance)
    )
  
  nir_data <- tidydata %>% 
    filter(Wavelength >= nir[1] & Wavelength <= nir[2]) %>% 
    group_by(Site, Block, Treatment, Date, Measurement) %>% 
    summarise(
      nir = mean(Reflectance)
    )
  
  ndvi_data <- inner_join(nir_data, red_data) %>% 
    mutate(ndvi = (nir-red)/(red+nir))
  
  return(ndvi_data)
}

calculate_index <- function(tidydata, band_defn ="MODIS", indices = c("NDVI", "EVI", "PBI_550")) {
  # Default MODIS bands for Red, NIR, Blue
  
  if (band_defn == "ITEX") {
    red <- c(560, 680)
    nir <- c(725, 1000)
  } else if (band_defn == "MODIS") {
    
    red <- c(620, 670)
    nir <- c(841, 876)
    blue <- c(459, 479)
  } else if (band_defn == "SKYE" ) {
    red <- c(620, 680)
    nir <- c(830, 880)
    blue <- c(455, 480)
  } else if (band_defn == "Toolik-GIS-drone") {
    # 2018 values
    red <- c(640,680)
    nir <- c(820,890)
    
    } else {
    print("ERROR - specify band definition")
  }

 index1_data <- tidydata %>%
    mutate(color = ifelse(Wavelength >= blue[1] & Wavelength <= blue[2], "blue", 
                          ifelse(Wavelength >= red[1] & Wavelength <= red[2], "red", 
                                 ifelse(Wavelength >= nir[1] & Wavelength <= nir[2], "nir",
                                        "other")))) %>% 
    group_by(Site, Block, Treatment, Date, Measurement, color) %>% 
    summarize(Reflectance = mean(Reflectance)) %>% 
    spread(color, Reflectance) %>% 
    mutate(NDVI = (nir-red)/(nir+red),
           EVI = 2.5*((nir-red)/(nir+6*red-7.5*blue+1)),
           EVI2 = 2.5*((nir-red)/(nir+2.4*red + 1)))
 
 index2_data <- tidydata %>% 
   group_by(Site, Block, Treatment, Date, Measurement, FileNum) %>% 
   summarize(PRI_550 = (Reflectance[Wavelength == 550][1] - Reflectance[Wavelength == 531][1])/ 
               (Reflectance[Wavelength == 550][1] + Reflectance[Wavelength == 531][1]),
             PRI_570 = (Reflectance[Wavelength == 570][1] - Reflectance[Wavelength == 531][1])/ 
               (Reflectance[Wavelength == 570][1] + Reflectance[Wavelength == 531][1]),
             WBI = Reflectance[Wavelength == 900][1] / Reflectance[Wavelength == 970][1],
             Chl = (Reflectance[Wavelength == 750][1] - Reflectance[Wavelength == 705][1])/ 
               (Reflectance[Wavelength == 750][1] + Reflectance[Wavelength == 705][1]))

  
 index_data <- inner_join(index1_data, index2_data) %>% 
   select(-c(red, blue, nir, other))
 
 return(index_data)
}


# NDVI = (NIR-Red)/(NIR+Red)
# 
# EVI = 2.5*((NIR-Red)/(NIR+6*Red-7.5*Blue+1))
# 
# EVI2 = 2.5*((NIR-Red)/(NIR+2.4*Red+1))
# 
# PRI (550 reference) = (550nm-531nm)/(550nm+531nm)
# 
# PRI (570 reference) = (570nm-531nm)/(570nm+531nm)
# 
# WBI = 900nm/970nm
# 
# Chl Index = (750nm-705nm)/(750nm+705nm)



# DIRECTORY ---------------------------------------------------------------

# Useful vectors for filtering rows, due to multiple naming conventions of LTER sites
WSG <- c("WSG1", "WSG23")
SHB <- c("SHB1", "SHB2", "SHRB")
HST <- c("HST", "HIST")
LOF <- c("LOF", "LMAT") 
site_list <- c("MAT", LOF, "MNAT", "NANT", "DHT", WSG, SHB, "HST")

CT <- c("CT","CT1","CT2")
NP <- c("F0.5","F1","F2","F5","F10","NP", "NO3", "NH4")
trtmt_list <- c(CT, "N", "P", NP)

# Read in Data from 2016-2017 ------------------------------------------------------------
data_path <- "LTER_R/"
# read in index data from summary
index_summary <- read_csv(file =paste0(data_path,"unispec_index_summary_2007-2016.csv"), col_names=T, col_type=cols(
  SCAN_ID = col_character(),
  Year = col_integer(),
  Date = col_date(format="%m/%d/%Y"),
  DOY = col_character(),
  Time = col_character(),
  Location = col_character(),
  Site = col_character(),
  Block = col_integer(),
  Treatment = col_character(),
  Measurement = col_integer(),
  NDVI_MODIS = col_double(),
  EVI_MODIS = col_double(),
  EVI2_MODIS = col_double(),
  PRI_550 = col_double(),
  PRI_570 = col_double(),
  WBI = col_double(),
  Chl = col_double(),
  LAI = col_double()
))

indices <- index_summary %>% 
  mutate(DOY = as.integer(DOY)) %>% 
  select(-SCAN_ID) %>% 
  rename(NDVI = NDVI_MODIS,
         EVI = EVI_MODIS,
         EVI2 = EVI2_MODIS)


# Load data from 2006-2018 ------------------------------------------------
load("LTER_R/unispec_index_summary_dataframe.Rda")


# Load data from 2017  -------------------------------------------------
load(paste0(data_path, "multispec_data_2017.Rda"))

tidy_multispec_data_2017 <- multispec_data_2017 %>% filter(Type=="correct") %>% 
  filter(Treatment %in% trtmt_list)

indices_2017 <- calculate_index(tidy_multispec_data_2017) %>%  
  ungroup() %>% 
  mutate(Year = as.integer(lubridate::year(Date))) %>% 
  mutate(Block = as.integer(str_extract(Block, "\\d"))) %>% 
  mutate(Measurement = as.integer(Measurement)) %>% 
  mutate(DOY = as.integer(lubridate::yday(Date))) 

# Load data from 2018  -------------------------------------------------
load(paste0(data_path,"multispec_data_2018.Rda"))

tidy_multispec_data_2018 <- multispec_data_2018 %>% filter(Type=="correct") %>% 
  filter(Treatment %in% trtmt_list)

indices_2018 <- calculate_index(tidy_multispec_data_2018) %>%  
  ungroup() %>% 
  mutate(Year = as.integer(lubridate::year(Date))) %>% 
  mutate(Block = as.integer(str_extract(Block, "\\d"))) %>% 
  mutate(Measurement = as.integer(Measurement)) %>% 
  mutate(DOY = as.integer(lubridate::yday(Date))) 


# Join Data ---------------------------------------------------------------

index_data <- bind_rows(indices, indices_2017, .id=NULL) %>% 
  bind_rows(indices_2018, .id = NULL) %>% 
  mutate(Site = replace(Site, Site %in% WSG, "WSG")) %>% # tidy : combine WSG1 & WSG23
  mutate(Site = replace(Site, Site %in% SHB, "SHB")) %>% 
  mutate(Site = replace(Site, Site %in% HST, "HST")) %>% 
  mutate(Site = replace(Site, Site %in% LOF, "LOF")) %>% 
  mutate(Site = replace(Site, Site == "HTH", "DHT"))

save(index_data, file =  "LTER_R/unispec_indices_summary_dataframe.Rda")

# Plot Data ---------------------------------------------------------------

index_byblock <- index_data %>% 
  filter(Treatment %in% trtmt_list) %>%
  #filter(Site == "MAT") %>% #Test line to look at subset of data
  mutate(Year = factor(Year)) %>% 
  mutate(Block = factor(Block)) %>% 
  group_by(Year, DOY, Date, Site, Block, Treatment) %>% 
  summarize_at(vars(NDVI:LAI), mean, na.rm=T) %>% 
  group_by(Year, DOY, Date, Site, Treatment) %>% 
  group_by(N = n(), add = TRUE) %>% # add number of blocks per site to get Standard Error
  summarize_at(vars(NDVI:LAI), funs(mean, sd), na.rm=T) 


which_index <- "NDVI"

index_tograph <- index_byblock %>% #Choose index to graph
  rename_at(vars(contains(which_index)), funs(sub(which_index, 'index', .)))

pur_pal <- RColorBrewer::brewer.pal(5, "Purples")

## Plot Site vs Year -- Treatment as colors 
ggplot(data = index_tograph, mapping = aes(x = DOY, y = index_mean, color=Treatment)) +
  geom_point() + 
  geom_line() + 
  geom_errorbar(aes(ymin = index_mean - index_sd/sqrt(N) , ymax= index_mean + index_sd/sqrt(N))) + 
  scale_color_manual(values=c("CT" = "black", "CT1"="black", "CT2"="black",
                              "N" = "blue2", "NO3" = "dodgerblue", "NH4" = "deepskyblue",
                              "P" = "red2",
                              "NP" = "green4",
                              "F0.5" = pur_pal[1],
                              "F1" = pur_pal[2],
                              "F2" = pur_pal[3],
                              "F5" = pur_pal[4],
                              "F10" = pur_pal[5]))  + 
  labs(y = which_index) + 
  facet_grid(Site ~ Year) 

## Plot Treatment vs. Year -- Site as colors, mostly just CONTROL

site_comp_CT <- index_tograph %>% 
  filter(Treatment %in% CT) 
ggplot(data = site_comp_CT, mapping = aes(x = DOY, y = index_mean, color=Site)) +
  geom_point() + 
  geom_line(aes(linetype=Treatment)) + 
  geom_errorbar(aes(ymin = index_mean - index_sd/sqrt(N) , ymax= index_mean + index_sd/sqrt(N))) + 
  labs(y = which_index) +
  facet_grid(. ~ Year)
