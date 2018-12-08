## SCRIPT to TEST R CODE that won't be in real scripts / may be packaged in functions, etc.
## AUTHOR: Ruby An
## DATE: 14 June 2018



# REQUIRED PACKAGES -------------------------------------------------------
require(tidyverse)
require(stringr)
# uses dplyr, tidyr, readr, ggplot2


# Directory -----------------------------------------------------------
prefix <- "Toolik-Summer-data/Toolik2017/Unispec/UnispecData/"
folder <- "Unispec1"
data_path <- paste0(prefix, folder)


# 1. Read in File Key --------------------------------------------------------

# Find all file keys 
key_files <- list.files(path = data_path, pattern = "*_key.csv*", full.names = T, recursive = T)

# Choose which key
key_file <- key_files[1]

key_csv <- read_csv(file = key_file, col_names = T,
                    col_types = cols(
                      Site = col_character(),
                      Block = col_character(),
                      Treatment = col_character(),
                      Date = col_character(),
                      Measurement1 = col_integer(),
                      Measurement2 = col_integer(),
                      Measurement3 = col_integer(),
                      Measurement4 = col_integer(),
                      Measurement5 = col_integer()
                    )) 

# Consolidate measurements to tidy dataframe
key_df <- key_csv %>% 
  gather(Measurement, FileNum, Measurement1:Measurement5) %>% 
  filter(!is.na(FileNum)) %>% 
  mutate(Measurement = str_sub(Measurement, 12, 12)) %>% 
  mutate(Date = lubridate::mdy(Date))



# Multispec File Lists ----------------------------------------------------

# Create file lists (SITE-YEAR-MONTH-DAY-multispecstate.csv)
reflec_files <- list.files(path = data_path, pattern = "reflec*\\.csv", full.names = T, recursive = T)
correct_files <- list.files(path = data_path, pattern = "correct*\\.csv", full.names = T, recursive = T)

files <- c(reflec_files, correct_files)

# TEST : Read 1 multispec file ---------------------------------------------------
multi_file <- files[21] #correct_files[2]

type <- str_extract(multi_file, "raw|correct") 
date <- ymd(str_extract(multi_file, "[\\d]{4}[-][\\d]+[-][\\d]+"))

meta <- read_lines(multi_file, n_max=5, skip=0)
### Comma separated values: 
## Line 1 - reference files (if corrected)
## Line 2 - list of .spu files. 
## Line 3 - date # NOT ACCURATE
## Line 4 - time # NOT ACCURATE, multispec does something weird to times. In raw .spu files, they are accurate.
## Line 5 - temp / wind? # NOT USED

ref_files <- str_split(meta[1], pattern=",")[[1]] 
spu_files <- str_split(meta[2], pattern=",")[[1]][-1] #Unlist & remove "Wavelength" to get list of data files
dates <- str_trim(str_split(meta[3], pattern=",")[[1]][-1]) 
times <- str_split(meta[4], pattern=",")[[1]][-1]


## Parsing file name into variables, tag with type, remove .spu 
site_fileNum_type_date <- str_c(gsub("*.spu", "", spu_files), type, date, sep = "_")

## Read in what the file looks like, metadata in column name
multi_file_df <- read_csv(file = multi_file, skip = 6,
                          col_names = c("Wavelength", site_fileNum_type_date), 
                          col_types = cols(
                            .default = col_double(),
                            Wavelength = col_integer()))

## Tidy up data frame
tidy_df <- multi_file_df %>% 
  gather(-Wavelength, key = "site_num_type", value = "Reflectance") %>% 
  separate(site_num_type, into = c("Site", "FileNum", "Type", "Date"), sep = "_", convert = T) %>% 
  mutate(Date = ymd(Date))

## Join to File Key to get block, treatment, measurement
df <- inner_join(key_df, tidy_df) %>% 
  filter(!(Treatment %in% c("REF", "DARK"))) %>% # Exclude the reference & dark
  filter(Wavelength >=400 & Wavelength <= 1500) # Choose relevant wavelengths 






