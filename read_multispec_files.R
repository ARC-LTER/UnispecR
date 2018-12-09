## Read in multispec data files and plot
## AUTHOR: Ruby An
## DATE: 14 June 2018
## Revised: 18 September 2018 
## 10 Oct 2018 changed to relative paths.  Jim Laundre

# REQUIRED PACKAGES -------------------------------------------------------
require(tidyverse)
require(stringr)
require(lubridate)
# uses dplyr, tidyr, readr, ggplot2


# Directory -----------------------------------------------------------
data_path <- "UnispecData/"


# Functions ------------------------------------------
source("unispec_functions.R")
# for read_key_file()

read_multispec_file <- function(multi_file) {
  ## Reads single multispec generated file
  ## Parses file name for usable info, format: SITE_{YEAR-MONTH-DAY}_TYPE.csv, 
          ## where TYPE is "raw" or "correct"
  ## Reads in metadata from first 5 lines
  ## Reads in reflectance data (400-1100nm Wavelengths) from following lines 
  
  type <- str_extract(multi_file, "raw|correct") 
  date <- ymd(str_extract(multi_file, "[\\d]{4}[-][\\d]+[-][\\d]+")) # regex for date
  
  meta <- read_lines(multi_file, n_max=5, skip=0)
  ### Comma separated values: 
  ## Line 1 - reference files (if corrected)
  ## Line 2 - list of .spu files. 
  ## Line 3 - date # NOT ACCURATE
  ## Line 4 - time # NOT ACCURATE, multispec does something weird to times. In raw .spu files, they are accurate.
  ## Line 5 - temp / wind? # NOT USED
  
  ref_files <- str_split(meta[1], pattern=",")[[1]] 
  spu_files <- str_split(meta[2], pattern=",")[[1]][-1] #Unlist & remove "Wavelength" to get list of data files
  site <- str_extract(spu_files, "^(.*?)(?=_)") #str_split(spu_files, "_")[[1]][1]
  fileNums <- as.numeric(str_extract(spu_files, "[\\d]{4,5}"))
  #dates <- str_trim(str_split(meta[3], pattern=",")[[1]][-1]) 
  #times <- str_split(meta[4], pattern=",")[[1]][-1]
  
  ## Parsing file name into variables, tag with type and date
  site_fileNum_type_date <- str_c(site, fileNums, type, date, sep = "_")
  
  ## Read in what the file looks like, metadata in column name
  multi_file_df <- read_csv(file = multi_file, skip = 6,
                            col_names = c("Wavelength", site_fileNum_type_date), 
                            col_types = cols(
                              .default = col_double(),
                              Wavelength = col_integer()))
  
  ## Tidy up data frame
  tidy_df <- multi_file_df %>% 
    gather(-Wavelength, key = "site_fileNum_type_date", value = "Reflectance") %>% 
    separate(site_fileNum_type_date, into = c("Site", "FileNum", "Type", "Date"), sep = "_", convert = T) %>% 
    mutate(Date = ymd(Date))
  
  return(tidy_df)
}


calculate_ndvi_multispec <- function(tidydata) {
  nir <- c(820, 890)
  red <- c(640, 680)
  
  red_data <- tidydata %>% 
    filter(Wavelength >= red[1] & Wavelength <= red[2]) %>% 
    group_by(Site, Block, Treatment, Date, Measurement, Type) %>% 
    summarise(
      red = mean(Reflectance)
    )
  
  nir_data <- tidydata %>% 
    filter(Wavelength >= nir[1] & Wavelength <= nir[2]) %>% 
    group_by(Site, Block, Treatment, Date, Measurement, Type) %>% 
    summarise(
      nir = mean(Reflectance)
    )
  
  ndvi_data <- inner_join(nir_data, red_data) %>% 
    mutate(ndvi = (nir-red)/(red+nir))
  
  return(ndvi_data)
}



# Useful vectors for filtering rows
WSG <- c("WSG1", "WSG23", "WSG")
SHB <- c("SHB1", "SHB2", "SHB")
site_list <- list("MAT", "LMAT", "MNAT", "NANT", "DHT", WSG, SHB, "HST")

CT <- c("CT","CT1","CT2")
NP <- c("F0.5","F1","F2","F5","F10","NP", "NO3", "NH4")
trtmt_list <- list(CT, "N", "P", NP)




# 1. Read in File Key --------------------------------------------------------
# This gives you the correspondence between each .spu file and the 
# site / block / plot / treatment / measurement at which it was taken. 

# Find all file keys 
key_files <- list.files(path = data_path, pattern = "*_key.csv*", full.names = T, recursive = T)

# Read in filekeys 
key_list <- data_frame(keyname = key_files) %>% # create dataframe
  mutate(key_contents = map(keyname, function(x) read_key_file(x)))


keys <- unnest(key_list) %>% 
  mutate(Site = replace(Site, Site %in% WSG, "WSG")) %>% 
  mutate(Site = replace(Site, Site %in% SHB, "SHB")) 


# 2. Multispec File Lists ----------------------------------------------------

# Create file lists (SITE-YEAR-MONTH-DAY-multispecstate.csv)
files <- list.files(path = data_path, pattern = "raw|correct", full.names=T, recursive = T)

# 3. Read in data ---------------------------------------------------------

data_list <- data_frame(filename = files) %>% # create dataframe
  mutate(file_contents = map(filename, function(x) read_multispec_file(x)))

data <- unnest(data_list) %>% 
  mutate(Site = replace(Site, Site %in% WSG, "WSG")) %>% 
  mutate(Site = replace(Site, Site %in% SHB, "SHB")) 

## Join to File Key to get block, treatment, measurement
keys_data <- inner_join(keys, data)

tidydata <- keys_data %>% 
  select(-c(filename, keyname)) %>% 
  #filter(!(Treatment %in% c("REF", "DARK"))) %>% # Exclude the reference & dark
  filter(Wavelength >=400 & Wavelength <= 1100) # Choose relevent wavelengths 



## Save data
#save(tidydata, file = "UnispecData/multispec_data_2017.Rda")




