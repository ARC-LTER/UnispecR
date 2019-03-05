## Read in multispec data files and plot
## AUTHOR: Ruby An
## DATE: 14 June 2018
## Revised: 18 September 2018 

# REQUIRED PACKAGES -------------------------------------------------------
require(tidyverse)
require(stringr)
require(lubridate)
# uses dplyr, tidyr, readr, ggplot2


# Directory -----------------------------------------------------------
data_path <- "Toolik-Summer-data/Unispec/Multispec/2018"


# Functions ------------------------------------------

read_multispec_file <- function(multi_file) { #DIFFERENT NAMING CONVENTION THAN 2017
  ## Reads single multispec generated file
  ## Parses file name for usable info, format: YEAR-MONTH-DAY_SITE_{BLOCKS_FILENUMS}_TYPE.csv
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

read_key_file <- function(key_file) {
  # Read in format of .csv
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
                        Measurement5 = col_integer(),
                        Weather = col_character(),
                        Notes = col_character()
                      )) 
  
  # Consolidate measurements to tidy dataframe
  key_df <- key_csv %>% 
    gather(Measurement, FileNum, Measurement1:Measurement5) %>% 
    filter(!is.na(FileNum)) %>% 
    mutate(Measurement = str_sub(Measurement, 12, 12)) %>% 
    mutate(Date = lubridate::mdy(Date))
  
  return(key_df)
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


keys <- unnest(key_list) # %>% 
  # mutate(Site = replace(Site, Site %in% WSG, "WSG")) %>% 
  # mutate(Site = replace(Site, Site %in% SHB, "SHB")) 


# 2. Multispec File Lists ----------------------------------------------------

# Create file lists (SITE-YEAR-MONTH-DAY-multispecstate.csv)
files <- list.files(path = data_path, pattern = "raw|correct", full.names=T, recursive = T)

# 3. Read in data ---------------------------------------------------------

data_list <- data_frame(filename = files) %>% # create dataframe
  mutate(file_contents = map(filename, function(x) read_multispec_file(x)))

data <- unnest(data_list) #%>% 
  #mutate(Site = replace(Site, Site %in% WSG, "WSG")) %>% 
  #mutate(Site = replace(Site, Site %in% SHB, "SHB")) 

## Join to File Key to get block, treatment, measurement
keys_data <- inner_join(keys, data)

multispec_data_2018 <- keys_data %>% 
  select(-c(filename, keyname)) %>% 
  #filter(!(Treatment %in% c("REF", "DARK"))) %>% # Exclude the reference & dark
  filter(Wavelength >=400 & Wavelength <= 1100) # Choose relevent wavelengths 


save(multispec_data_2018, file = "Toolik-Summer-data/Unispec/multispec_data_2018.Rda")









# OBSELETE??? 4. Plot Reflectance Data ------------------------------------------------------------

# Color sequences
pur_pal <- RColorBrewer::brewer.pal(5, "Purples")

####UNFISIHED
# SELECTION
sites <-c("SHB")
blocks <- c("B1", "B2")
trtmts <- c(NP, CT, "N", "P") 
types <- c("correct")

# Plotting Format
plotdata <- tidydata %>% 
  filter(Site %in% sites) %>% 
  filter(Block %in% blocks) %>% 
  filter(Treatment %in% trtmts) %>% 
  filter(Type %in% types) %>% 
  group_by(Site, Block, Treatment, Date, Wavelength) %>% 
  summarize(
    avg_reflect = mean(Reflectance),
    max_ref = max(Reflectance),
    min_ref = min(Reflectance),
    var = var(Reflectance)) 

# Actual Plot 
ggplot(data = plotdata, mapping = aes(x = Wavelength, y =avg_reflect)) +
  #geom_ribbon(aes(ymin=min_ref, ymax=max_ref, fill=Treatment), alpha=0.25) +
  geom_line(aes(color=Treatment)) +
  facet_grid(Block ~ Date) +
  scale_color_manual(values=c("CT" = "black", "CT1"="black", "CT2"="black",
                              "N" = "blue2", "NO3" = "dodgerblue", "NH4" = "deepskyblue",
                              "P" = "red2",
                              "NP" = pur_pal[5],
                              "F0.5" = pur_pal[1],
                              "F1" = pur_pal[2],
                              "F2" = pur_pal[3],
                              "F5" = pur_pal[4],
                              "F10" = pur_pal[5]))  +
  scale_fill_manual(values=c("CT" = "black", "CT1"="black", "CT2"="black",
                             "N" = "blue2", "NO3" = "dodgerblue", "NH4" = "deepskyblue",
                             "P" = "red2",
                             "NP" = pur_pal[5],
                             "F0.5" = pur_pal[1],
                             "F1" = pur_pal[2],
                             "F2" = pur_pal[3],
                             "F5" = pur_pal[4],
                             "F10" = pur_pal[5]))


# OBSELETE??? 5. Calculate NDVI  ------------------------------------------------------

#### NOW OBSELETE???? 
#### LOOK IN unispec_indices_summary for the real deal. 


# SELECTION
sites <-c("LMAT")
blocks <- c("B1", "B2", "B3", "B4")
trtmts <- c(NP, CT, "N", "P") 
types <- c("correct")

# subset of full dataframe
sub_data <- tidydata %>% 
  filter(Site %in% sites) %>% 
  filter(Block %in% blocks) %>% 
  filter(Treatment %in% trtmts) %>% 
  filter(Type %in% types) 
  

ndvi_data <- calculate_ndvi_multispec(sub_data) 

plot_data <- ndvi_data %>% 
  group_by(Site, Block, Treatment, Type, Date) %>% 
  summarise(
    avg_ndvi = mean(ndvi),
    sd_ndvi = sd(ndvi)
  )


## Bar Plot
ggplot(data = plot_data, mapping = aes(x = Treatment, y = avg_ndvi, fill=Type)) +
  geom_bar(stat="identity", position= position_dodge(), color="black") + 
  geom_errorbar(aes(ymin = avg_ndvi-sd_ndvi, ymax= avg_ndvi + sd_ndvi), width=0.2, position = position_dodge(0.9)) +
  facet_grid(Block ~ Date)


## Line Plot
ggplot(data = plot_data, mapping = aes(x = Date, y = avg_ndvi, color = Treatment)) +
  geom_point(aes(shape=Type)) + 
  geom_line(aes(linetype=Type)) +
  geom_errorbar(aes(ymin = avg_ndvi-sd_ndvi, ymax= avg_ndvi + sd_ndvi)) + 
  scale_color_manual(values=c("CT" = "black", "CT1"="black", "CT2"="black",
                              "N" = "blue2", "NO3" = "dodgerblue", "NH4" = "deepskyblue",
                              "P" = "red2",
                              "NP" = pur_pal[5],
                              "F0.5" = pur_pal[1],
                              "F1" = pur_pal[2],
                              "F2" = pur_pal[3],
                              "F5" = pur_pal[4],
                              "F10" = pur_pal[5]))  +
  facet_grid(Block ~ Site)


