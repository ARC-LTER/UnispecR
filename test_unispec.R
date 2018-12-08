## Script for testing Unispec fiberoptic cables & reference disc
## AUTHOR: Ruby An
## DATE: 6/24/2017

# REQUIRED PACKAGES -------------------------------------------------------

library('tidyverse')
library('stringr')


# FUNCTIONS ---------------------------------------------------------------

read_spu_file <- function(fileName) {
  con <- file(fileName, "r")
  text <- readLines(con, n=9)
  close(con)
  
  fileInfo <- strsplit(fileName, split = "/|_|\\.") #split up filename into components
  data <- read.table(file = fileName, skip = 9, col.names = c("Wavelength", "ChB", "ChA")) %>% 
    mutate(Reflectance = ChB/ChA) 
  
  data$Date <- as.Date(fileInfo[[1]][1], "%Y-%m-%d")
  data$Site <- "TEST"
  data$FileNum <- as.numeric(str_sub(fileName,11,15)) #as.integer(gsub("^.*_([0-9]+)\\..*","\\1", fileName))
  data$Time <- format(strptime(substr(text[3], 14, gregexpr(pattern="\"", text[3])[[1]][2]-1), "%m/%d/%Y %I:%M:%S %p"), format="%H:%M:%S")
  
  data$int <- as.numeric(strsplit(text[8], split = " ")[[1]][3])
  # data$Temp <- as.numeric(strsplit(strsplit(text[5], split = " ")[[1]][4], split="=")[[1]][2])
  
  # where interpolation needs to happen
  # add correction factor column, mutate to get corrected reflectance 
  
  data <- data %>% filter(Wavelength >= 400 & Wavelength <= 1125)  
  
  return(data)
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

# Generate File List  -----------------------------------------------------------
folder <- "Toolik-Summer-data/Toolik2018/Unispec/TEST"
setwd(folder)

# Specify which site and what filenumbers you want to analyze
siteID <- "2018*" #regular expression for which files you want
first <- 0 # use these to restrict the file numbers you are analyzing 
last <- 1000

fileList <- list.files(path = folder, pattern = siteID, full.names = FALSE, recursive=T)
fileNums <- as.numeric(str_sub(fileList,11,15))# as.numeric(gsub("^.*_([0-9]+)\\..*","\\1", fileList))
subfileNums <- fileNums >= first & fileNums <= last
files <- fileList[subfileNums]

# Read in Data from files------------------------------------------------------------
data_list <- data_frame(filename = files) %>% # create dataframe
  mutate(file_contents = map(filename, function(x) read_spu_file(x)))

all_data <- unnest(data_list)


# Read File Key -------------------------------------------------------

key <- read_key_file("UnispecTEST_key.csv")

df <- inner_join(key, all_data) %>% gather(channel, intensity, ChB:ChA)


# Plot reflectance for subset of fileList
ggplot(data = plotdata, mapping = aes(x = Wavelength, y = Reflectance, color=Site)) +
  geom_line(aes()) 
#geom_line(data = add_data, aes(color=Site, linetype=channel)) 
#geom_line(data = add_data_2, aes(color=Site), linetype=3)



## July 27, 2017 Test 
## Read in select files // quality check ## REFERENCES 
subfiles <- list.files(path = data_path, pattern = "REF", full.names = TRUE, recursive=T)

subdata_list <- data_frame(filename = subfiles) %>% # create dataframe
  mutate(file_contents = map(filename, function(x) read_spu_file(x)))

subdata <- unnest(subdata_list) %>% 
  filter(Date == "2017-07-27") %>% 
  mutate(int = factor(int)) %>% 
  filter(!(FileNum %in% c(0, 41, 42))) %>% 
  filter(FileNum < 40) %>% 
  filter(Wavelength > 400 & Wavelength < 1100) %>% 
  group_by(Site, Wavelength, Date, int) %>% 
  summarize(Correction = 1/mean(Reflectance))

ggplot(data = subdata, mapping = aes(x = Wavelength, y = Correction, color = int)) +
  geom_line(aes())
