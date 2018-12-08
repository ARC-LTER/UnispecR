## Unispec Data Processing Pipeline
### Quality Check / Assurance of unispec data files <.spu> 
### 1. Check white reference files by plotting, choose desired files for Unipsec Key 
### 2. Identify Max'd out spectra 

## AUTHOR: Ruby An
## DATE: 2018-06-24
## REVISED: 2018-11-29

# REQUIRED PACKAGES -------------------------------------------------------
require(tidyverse)
require(stringr)
require(lubridate)
# uses dplyr, tidyr, readr, ggplot2

# FUNCTIONS ---------------------------------------------------------------
source("unispec_functions.R")

# SET DIRECTORY ------------------------------------------------------------
data_path <- getwd()

## Useful vectors for standardizing names and filtering rows
WSG <- c("WSG1", "WSG23")
SHB <- c("SHB1", "SHB2")
site_list <- list("MAT", "LMAT", "MNAT", "NANT", "DHT", WSG, SHB, "HST")

CT <- c("CT","CT1","CT2")
NP_gradient <- c("F0.5","F1","F2","F5","F10") # for LOF site
N_types <- c("NO3", "NH4") # for LOF site
trtmt_list <- list(CT, "N", "P", "NP", NP_gradient, N_types)


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

# 2. Read .spu DATA ---------------------------------------------------------

## Find .spu files
### alter data_path or specify Date(s) or Site(s) to read files
files <- list.files(path = data_path, pattern = ".spu$", full.names = FALSE, recursive=T)

## Read data from files 
data_list <- data_frame(filename = files) %>% # create dataframe
  mutate(file_contents = map(filename, function(x) read_spu_file(x)))

## Unpack into usable dataframe 
data <- unnest(data_list) 

# 3. Join DATA & KEYS --------------------------------------------------------

## Join all data matching keys dataframe (drops any non-matching data),
##  by columns (Date, Site, FileNum)
df <- inner_join(data, keys) %>% 
  filter(Wavelength >= 500 & Wavelength <= 1000) # Choose relevent wavelengths 


# 4. CHECK references --------------------------------------------------
### White Refs corrects for instrument & cable irregularities
### Multiplying by correction factor smooths out the spectra. 

## Find all white reference files 
ref_data <- df %>% 
  filter(str_detect(Treatment, "REF"))%>% # extract reference data 
  separate(Block, into = c("BX1", "BX2", "BX3", "BX4"), sep = ",") %>% # expand string entry in "Block", necessary if different ref for different blocks at same site
  gather(Block, BlockString, BX1:BX4) %>% # re-condense into one column
  mutate(Block = str_squish(BlockString), BlockString=NULL) %>% # remove whitespace from Block names introduced by "separate" function
  filter(!is.na(Block)) %>% # remove empty rows for site w/out B3 or B4
  mutate(CorrectionFactor_REF = ChA/ChB)

## Plot ALL Correction Factors for quality check 
ggplot(data = ref_data, mapping = aes(x = Wavelength, y = CorrectionFactor_REF)) +
  geom_line(aes(color=Treatment, linetype=Measurement, alpha=Block)) +
  facet_grid(Site ~ Date) + 
  scale_alpha_discrete(range=c(1, 0.5)) # transparency by block

## Choose references (usually 5 measurements per date/site)
sites <- unlist(site_list)
sites <- sites[which(!(sites %in% c("MNAT", "SHB1")))] # not MNAT or SHB1
dates <- df %>% select(Date) %>% unique() %>% slice(1:n()) %>% c()
dates <- dates[[1]]

ref_check <- ref_data %>% 
  filter(Date %in% dates) %>% 
  filter(Site %in% sites)  %>%  
  ### FURTHER SPECIFY EXACT SITE/DATE/ETC to INVESTIGATE FURTHER
  filter(Site == "LMAT")

## Replot Specified Correction Factors for quality check 
ggplot(data = ref_check, mapping = aes(x = Wavelength, y = CorrectionFactor_REF)) +
  geom_line(aes(color=Treatment, linetype=Measurement, alpha=Block)) +
  facet_grid(Site ~ Date) + 
  scale_alpha_discrete(range=c(1, 0.25))

## EXAMINE SPECIFIC info: check Integration, Notes, Weather, filename, etc. 
ref_check_files <- ref_check %>% 
  select(-c(Wavelength, ChA, ChB, CorrectionFactor_REF, keyname, Time)) %>% 
  unique()

### View dataframe of ref_check_files
ref_check_files 


# 5. CHOOSE references & CHECK  -------------------------------------------------------
## Based on the above REF quality check plots, 
##  change Treatment to REF for the Files you want to use 
##  in the appropriate "YEAR_unispec_key.csv" file

## Re-read in filekeys, assume same key_files list as before
key_list <- data_frame(keyname = key_files) %>% # create dataframe
  mutate(key_contents = map(keyname, function(x) read_key_file(x))) 

## Unpack into usable dataframe & joins w/data
keys <- unnest(key_list) 

df <- inner_join(data, keys) %>% 
  filter(Wavelength >= 500 & Wavelength <= 1000) # Choose relevent wavelengths 

## Find & choose reference data
ref_data <- df %>% 
  filter(Treatment == "REF") %>% # NOTE CHANGE From "str_detect()" to "==" 
  separate(Block, into = c("BX1", "BX2", "BX3", "BX4"), sep = ",") %>% # expand string entry in "Block", necessary if different ref for different blocks at same site
  gather(Block, BlockString, BX1:BX4) %>% # re-condense into one column
  mutate(Block = str_squish(BlockString), BlockString=NULL) %>% # remove whitespace from Block names introduced by "separate" function
  filter(!is.na(Block)) %>% # remove empty rows for site w/out B3 or B4
  mutate(CorrectionFactor_REF = ChA/ChB)

## Summarize 5 chosen references measurements (and integrations)
ref_summary <- ref_data %>% 
  group_by(Date,Site,Block,Wavelength) %>% # group_by(Date,Site,Block,Wavelength, int) %>% 
  summarize(ChA_REF = mean(ChA), ChB_REF = mean(ChB), CorrectionFactor_REF = mean(ChA/ChB), int_REF = mean(int), Notes_REF = list(Notes))

# CHECK ALL unique DATES/SITES/BLOCKS ARE ACCOUNTED FOR POST SUMMARY
cover_check <- ref_data %>%
  select(Date, Site, Block, Wavelength) %>%
  unique() %>%
  arrange(Site, Block, Wavelength)

if(nrow(cover_check) == nrow(ref_summary)){print("ALL IS WELL. ref_summary is comprehensive.")}

# JOIN with DATA 
## Tidy dataframe 
df_tidy <- df %>% 
  mutate(Site = replace(Site, Site %in% WSG, "WSG")) %>% # tidy : combine WSG1 & WSG23
  mutate(Site = replace(Site, Site %in% SHB, "SHB"))  # tidy : combine SHB1 & SHB2 

df_ref <- inner_join(df_tidy, ref_summary) %>% 
  select(Date, Time, Site, Block, Treatment, Measurement, Wavelength,  int, int_REF, ChB, ChA, ChB_REF, ChA_REF, CorrectionFactor_REF, Weather, Notes, Notes_REF, filename, FileNum, keyname) %>%
  mutate(raw = ChB/ChA) %>% 
  mutate(correct = raw*CorrectionFactor_REF) %>% 
  gather(Type, Reflectance, raw:correct)


# CHECK ALL MEASUREMENTS ARE ACCOUNTED FOR post join 
cover <- df_ref %>%
  select(Date, Site, Block,Treatment,Measurement, Wavelength) %>%
  unique() %>%
  arrange(Date, Site, Block, Treatment, Measurement)

if(nrow(cover)*2 == nrow(df_ref)) {print("ALL IS WELL. df_ref is pure.")}


# 6. QUALITY CHECK FILES -------------------------------------------------------------

### WRITE THIS SECTION WHEN ACTUALLY QUALITY CHECKING DATA
## TimeCheck files for correcting key

# SPECIFY DESIRED FOLDER
data_path <- "/UnispecData/Unispec3/27JUN2017/" 

# Specify which site and what filenumbers you want to analyze
siteID <- "^MNAT_*" #regular expression for which files you want
first <- 0
last <- 5000


fileList <- list.files(path = data_path, pattern = siteID, full.names = FALSE, recursive=T)
fileNums <- as.numeric(gsub("^.*_([0-9]+)\\..*","\\1", fileList))
subfileNums <- fileNums >= first & fileNums <= last
files <- fileList[subfileNums]

# Read in Data
data_list <- data_frame(filename = files) %>% # create dataframe
  mutate(file_contents = map(filename, function(x) read_spu_file(x)))

data <- unnest(data_list) 

# Select columns to Check
timedata <- data %>% select(Site, Date, FileNum, Time, int) %>% 
  group_by(Time) %>% distinct() 
timedata$diff <- timedata$Time - lag(timedata$Time)
time_check <- timedata %>% select(Site, Time, FileNum, diff, int) #%>% filter(FileNum>=0 & FileNum <=15)

# Examine dataframe
time_check


## PLOT Specific Files 
plotdata <- data %>% 
  filter(Wavelength > 500 & Wavelength < 1000) %>% 
  ## Choose file subset
  filter(FileNum >= 0 & FileNum <= 20) %>% 
  mutate(FileNum = factor(FileNum))

ggplot(data=plotdata, aes(x=Wavelength, y=Reflectance)) +
  geom_line(aes(color=FileNum)) +
  facet_wrap(~FileNum)


## Check for Max'd out Spectra
bad_data <- data %>% filter(ChA > 65000) %>% 
  group_by(Date, Site, FileNum) %>% unique()

