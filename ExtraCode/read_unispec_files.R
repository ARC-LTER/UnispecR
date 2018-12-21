## Arctic LTER Unispec Data Processing Pipeline
### 1. Reads in unispec key files <_key.csv>
### 2. Reads in unispec data files <.spu> 
### 3. Applies white reference correction
### 4. Plot & Save spectra (if desired)
### 5. Calculates spectral indices (NDVI, etc.) 

## AUTHOR: Ruby An
## DATE: 2018-06-24
## REVISED: 2018-11-29

#### VEGETATION INDEX DEFINITIONS 
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
# uses dplyr, tidyr, readr, ggplot2

## Useful vectors for standardizing names and filtering rows
WSG <- c("WSG1", "WSG23")
SHB <- c("SHB1", "SHB2")
site_list <- list("MAT", "LMAT", "MNAT", "NANT", "DHT", WSG, SHB, "HST")

CT <- c("CT","CT1","CT2")
NP_gradient <- c("F0.5","F1","F2","F5","F10")
N_types <- c("NO3", "NH4")
trtmt_list <- list(CT, "N", "P", "NP", NP_gradient, N_types)

# SET DIRECTORY ------------------------------------------------------------
data_path <- getwd()

# FUNCTIONS ---------------------------------------------------------------
source("unispec_functions.R")

# 1. Read in KEYS ---------------------------------------------------------

## Find all file keys 
### alter search pattern to choose specific key(s)
key_files <- list.files(path = data_path, pattern = "*_key.csv", full.names = T, recursive = T)

## Read in data from filekeys 
key_list <- data_frame(keyname = key_files) %>% # create dataframe
  mutate(key_contents = map(keyname, function(x) read_key_file(x))) 
    # map function: super useful! to apply function to objects without slow for loops

## Unpack into usable dataframe 
keys <- unnest(key_list)

# > 1.5 Read .spu DATA ---------------------------------------------------------
# YOU CAN SOMETIMES SKIP THIS SECTION IF YOU'VE JUST RUN QAQC 
#  and <data> is still in your environment)

## Find .spu files
### alter data_path directory or search pattern to specify files 
files <- list.files(path = data_path, pattern = ".spu$", full.names = FALSE, recursive=T)

## Read data from files 
data_list <- data_frame(filename = files) %>% # create dataframe
  mutate(file_contents = map(filename, function(x) read_spu_file(x)))

## Unpack into usable dataframe 
data <- unnest(data_list) 

# 2. Join DATA & KEYS --------------------------------------------------------

## Join all data matching keys dataframe (drops any non-matching data),
##  by columns (Date, Site, FileNum)
df <- inner_join(data, keys) %>% 
  filter(Wavelength >= 500 & Wavelength <= 1000) # Choose relevent wavelengths 


# 3. Apply REFerences --------------------------------------------------
## MAKE SURE YOU'VE QAQC'd to select correct REFS

## Find REF files for correction factors
ref_data <- df %>% 
  filter(Treatment == "REF") %>% # extract reference data 
  separate(Block, into = c("BX1", "BX2", "BX3", "BX4"), sep = ",") %>% # expand string entry in "Block", necessary if different ref for different blocks at same site
  gather(Block, BlockString, BX1:BX4) %>% # re-condense into one column
  mutate(Block = str_squish(BlockString), BlockString=NULL) %>% # remove whitespace from Block names introduced by "separate" function
  filter(!is.na(Block)) # remove empty rows for site w/out B3 or B4

## Summarize 5 chosen ref measurements per DATE/SITE/BLOCK 
ref_summary <- ref_data %>% 
  group_by(Date,Site,Block,Wavelength) %>% # group_by(Date,Site,Block,Wavelength, int) %>% 
  summarize(ChA_REF = mean(ChA), ChB_REF = mean(ChB), CorrectionFactor_REF = mean(ChA/ChB), int_REF = mean(int), Notes_REF = list(Notes))

## Join DATA with REFS 
df_ref <- inner_join(df_tidy, ref_summary) %>% 
  select(Date, Time, Site, Block, Treatment, Measurement, Wavelength,  int, int_REF, ChB, ChA, ChB_REF, ChA_REF, CorrectionFactor_REF, Weather, Notes, Notes_REF, filename, FileNum, keyname) %>%
  mutate(raw = ChB/ChA) %>% # this is the raw reflectance
  mutate(correct = raw*CorrectionFactor_REF) %>% # this line calculates corrected reflectance
  gather(Type, Reflectance, raw:correct)


# 4. Plot (& Save) Spectra ---------------------------------------------------------

# PLOT SELECTION
sites <-c("MAT", "LMAT", "HST")
blocks <- c("B1")
trtmts <- c(CT, "NP") 
measures <- c("1", "2", "3", "4", "5")

# Data Comparison Format
df_plot <- df_ref %>% 
  filter(Site %in% sites) %>% 
  filter(Block %in% blocks) %>% 
  filter(Treatment %in% trtmts) %>% 
  group_by(Site, Block, Treatment, Date, Wavelength) %>% 
  summarize(
    avg_reflect = mean(Reflectance),
    max_ref = max(Reflectance),
    min_ref = min(Reflectance),
    sd_reflect = sd(Reflectance)
    ) 

# Color sequences
pur_pal <- RColorBrewer::brewer.pal(5, "Purples")

ggplot(data = df_plot, mapping = aes(x = Wavelength, y = avg_reflect)) +
  geom_line(aes(color=Treatment)) + 
  geom_ribbon(aes(ymin=avg_reflect-sd_reflect, ymax=avg_reflect+sd_reflect, fill=Treatment), alpha=0.25) +
  facet_grid(Site ~ Date)


# >> 4.5 SAVE CORRECTED SPECTRA --------------------------------------------------
# SAVE CORRECTED SPECTRA if you want the dataframe for later
# unispec_data_2018 <- df_ref %>% 
#   filter(Type=="correct") %>% 
#   select(-Type)
# save(unispec_data_2018, file = "unispec_data_2018.Rda")



# 6. Calculate NDVI & Plot----------------------------------------------------------

# SELECTION
sites <-c("MAT")
blocks <- c("B1", "B2", "B3", "B4")
trtmts <- c(CT) 

ndvi_types <- df_ref %>% 
  filter(Site %in% sites) %>% 
  filter(Block %in% blocks) %>% 
  filter(Treatment %in% trtmts) %>% 
  calculate_ndvi()

ndvi_plot <- ndvi_types %>% 
  group_by(Site, Block, Treatment, Measurement, Date) %>% 
  summarise(
    avg_ndvi = mean(ndvi),
    sd_ndvi = sd(ndvi)
  )

ggplot(data = ndvi_plot, mapping = aes(x = Date, y = avg_ndvi, color = Treatment)) +
  geom_point() +
  geom_line() + 
  geom_errorbar(aes(ymin = avg_ndvi-sd_ndvi, ymax= avg_ndvi + sd_ndvi), width=2) + 
  # scale_color_manual(values=c("CT" = "black", "CT1"="black", "CT2"="black",
  #                             "N" = "blue2", "NO3" = "dodgerblue", "NH4" = "deepskyblue",
  #                             "P" = "red2",
  #                             "NP" = pur_pal[5],
  #                             "F0.5" = pur_pal[1],
  #                             "F1" = pur_pal[2],
  #                             "F2" = pur_pal[3],
  #                             "F5" = pur_pal[4],
  #                             "F10" = pur_pal[5]))  +
  facet_grid( Measurement ~ Block)



