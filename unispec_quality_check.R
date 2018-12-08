## Code for checking file number / key and integration time of .spu files.
## Description: Reads in .spu files based on file directory, regular expression. 
#### Creates dataframe of files with time of measurement, difference in time, and integration time

## Author: Ruby An
## Date: July 2018


# REQUIRED PACKAGES -------------------------------------------------------

library('tidyverse')
library('stringr')

read_spu_file <- function(fileName) {
  # Read metadata from first 9 lines of .spu file
  text <- read_lines(fileName, n_max = 9)
  
  # Get info from filename
  fileInfo <- str_split(text[1], pattern = c('\\\\'))[[1]]
  # works when files saved in default location on PCMIA card, not otherwise
  
  # Read spectral intensity data into dataframe
  data <- read.table(file = fileName, skip = 9, col.names = c("Wavelength", "ChB", "ChA")) %>% 
    mutate(Reflectance = ChB/ChA)
  
  # Tag with appropriate metadata
  data$Site <- str_extract(fileName, "(?<=/)(.*?)(?=_)") # get string after / & before _
  # for fileName format "DATE/SITE_FILENUM.spu", e.g. "04AUG2017/DHT_00000.spu"
  data$FileNum <- as.integer(str_extract(fileName, "[0-9]{5}"))
  data$Time <-  lubridate::mdy_hms(str_extract(text[3], "\\d+/\\d+/\\d{4}\\s\\d+:\\d{2}:\\d{2}\\s(PM|AM)"), tz="America/Anchorage")
  data$Date <- lubridate::date(data$Time)
  # lubridate::dmy(fileInfo[4]) # works when files saved in default location on PCMIA card
  data$int <- as.numeric(strsplit(text[8], split = " ")[[1]][3])
  data$Temp <- as.numeric(strsplit(strsplit(text[5], split = " ")[[1]][4], split="=")[[1]][2])
  
  return(data)
}

# QUALITY CONTROL ------------------------------------------------------------
folder <- "Toolik-Summer-data/Toolik2017/Unispec/UnispecData/Unispec3/27JUN2017/"
setwd(folder)

# Specify which site and what filenumbers you want to analyze
siteID <- "^MNAT_*" #regular expression for which files you want
first <- 0
last <- 50000


fileList <- list.files(path = folder, pattern = siteID, full.names = FALSE, recursive=T)
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

time_check


## PLOT Specific Files 
plotdata <- data %>% 
  filter(Wavelength > 400 & Wavelength < 1100) %>% 
  filter(FileNum >= 0 & FileNum <= 20) %>% 
  mutate(FileNum = factor(FileNum))
ggplot(data=plotdata, aes(x=Wavelength, y=Reflectance)) +
  geom_line(aes(color=FileNum)) +
  facet_wrap(~FileNum)


# Cable Check -------------------------------------------------------------

# 2018-07-30
cable <- c(NA, rep("old", 15), rep("new",20))
key <- c("DARK", rep(c("REF", "TURN", "DRYAS"), each = 5), rep(c("REF", "TURN", "TILT", "DRYAS"), each = 5))


df <- data_list

df$key <- key
df$cable <- cable
test_data <- unnest(df)

plotdata <- test_data %>% 
  filter(Wavelength > 400 & Wavelength < 1100) %>% 
  filter(key == "TURN") %>% 
  mutate(FileNum = factor(FileNum))

ggplot(data=plotdata, aes(x=Wavelength, y=Reflectance)) +
  geom_line(aes(color=cable, linetype = FileNum)) 
