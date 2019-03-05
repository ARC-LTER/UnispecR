## Read info from spu, process for files before 2017.  In those years there was not a separate key file
## for looking up the site information for the raw .spu files.  Another r script was used to rename the 
## multispec files and directories in a consistent way.  See issues.docx for a log of data issues and changes
## from the orginal files.
## Jim Laundre 2018-10-25
## Revised 2019-01-24  
# REQUIRED PACKAGES -------------------------------------------------------
require(tidyverse)
require(stringr)
require(lubridate)
require(openxlsx)
require(rChoiceDialogs)

#----------- FUNCTIONS ---------------------------------------------------------------

#***********Read files that were processed by Multispec and saved as excel file************
#   These files have reflectance files, site, treatment and block info.

read.processedfile <- function(filename) {
  
  # Read the first 21 rows of the "Spectra" sheet to get the metadata for the spu files
  #print(filename)  # Uncomment for testing which file is trouble.
  df_spectra <- read.xlsx(xlsxFile = filename, sheet = "Spectra", skipEmptyRows = T, rows = 1:21, detectDates = F) %>%
      set_tidy_names(.) %>%
      gather(columX,valname,-Reflectance) %>%
      spread(Reflectance,valname)%>%                # Transposed the first row to column variables
    dplyr::rename(Spufilename = Wavelength, Research_location = SITE, Site = EXPERIMENT, Date = DATE, Block = BLOCK, 
             Time = TIME, Treatment = TREATMENT) %>%  # Rename the columns
      select (-YEAR, -columX) %>%
      select_all(~gsub("\\s+|\\.", "", .)) %>%        # remove spaces from column names
      select( Research_location, Date, Time, Block, Site, Treatment, REP, Spufilename, 'NDVI(MODIS)',
              'EVI(MODIS)', 'EVI2(MODIS)','PRI(550Reference)', 'PRI(570Ref)',
              'WBI','ChlIndex')
      
  # Read first 30 rows of Notes sheet to get the metadata from a processed file
  
  df_references <- read.xlsx(xlsxFile = filename, sheet = "Notes", skipEmptyRows = T,rows = 1:30) %>%
    gather(columnx,valname, -Summary.Information) %>%
    spread(Summary.Information,valname) %>%        # Transposed the first row to column variables
    dplyr::rename(Research_location = SITE, Date = 'Date Collected',Spufilename ='White Panel Reflectance File Used',   #rename them to match df_specra 
           Site = 'LTER Experiment',Experiment = EXPERIMENT, Block = BLOCK, 
           Weather = 'Brief Weather Description', Time = 'Approximate Time of Collection')  %>%
    mutate(Site = ifelse(is.na(Site),str_replace(filename,".*[/](\\w*)[_]+.*$","\\1"), Site)) %>%
    arrange(Date) %>%  # If there are more then 9 columns of reference files then need to make sure the Date value is first 
    mutate (Research_location = Research_location[1], Date = Date[1], Time = Time[1], Site = Site[1], 
            Block = NA, Treatment = paste0("REF",row.names(.))) %>%
    select (Research_location, Date, Time, Site, Block, Treatment, Spufilename, Weather) %>%
    mutate(Spufilename = str_replace(Spufilename,"^.*[:*](.*$)","\\1")) %>%
    unnest(Spufilename = strsplit(Spufilename, "\\s*,\\s*")) %>%  # Spufilenames can be a comma delimited string of files; create a row for each
    mutate (Spufilename =ifelse(str_detect(Spufilename,".spu"), Spufilename, paste0(tolower(Spufilename),".spu")))
            
  
  # Append the references spectra to the measured spectra
  df_all <-bind_rows(df_references,df_spectra)
  df_all <- df_all %>%
   mutate(Date = convertToDate(Date,tz ="America/Anchorage"),   #convert excel data to date
            Spufilename = tolower(trimws(Spufilename)),
            FileNum = as.integer(str_replace(Spufilename,"(^.*?\\d{1,2})(\\D*)(\\d{5,7})(\\.spu$)","\\3")),
            Process_file = filename)
 
  return(df_all)  
}

read.process_spectrafile <- function(filename) {
  # For "Spectra" sheets that do not include the associated spu filenames.
  # Read the first 21 rows of the "Spectra" sheet to get the metadata for the spu files
 
  df_spectra <- read.xlsx(xlsxFile = filename, sheet = "Spectra", skipEmptyRows = T, rows = 1:21, detectDates = F) %>%
    set_tidy_names(.) %>%
    gather(columX,valname,-Reflectance) %>%
    spread(Reflectance,valname)%>%                # Transposed the first row to column variables
    dplyr::rename(Spufilename = Wavelength, Research_location = SITE, Site = EXPERIMENT, Date = DATE, Block = BLOCK, 
           Time = TIME, Treatment = TREATMENT) %>%  # Rename the columns
    select (-YEAR, -columX) %>%
    select_all(~gsub("\\s+|\\.", "", .)) %>%        # remove spaces from column names
    select( Research_location, Date, Time, Block, Site, Treatment, REP, Spufilename, 'NDVI(MODIS)',
            'EVI(MODIS)', 'EVI2(MODIS)','PRI(550Reference)', 'PRI(570Ref)',
            'WBI','ChlIndex')
  df_all <- df_spectra %>%
    mutate(Date = convertToDate(Date,tz ="America/Anchorage"),   #convert excel data to date
           Spufilename = tolower(trimws(Spufilename)),
           FileNum = as.integer(str_replace(Spufilename,"(^.*?\\d{1,2})(\\D*)(\\d{5,7})(\\.spu$)","\\3")),
           Process_file = filename)
  return(df_all)
}
#********************Read in the information from the spu files **************************************

read.spufile.metadata <- function(filename) {
  
  # Extract metadata from filenames that have format "DATE/SITE_FILENUM.spu", e.g. "2018-06-22/DHT_00000.spu"
  Site <- toupper(str_replace(filename,".*[/](.*)[_]+.*$","\\1")) # get string after / & before _
  
  # Extract metadata from filenames that have format "DATESITEFILENUM.spu", e.g. "JUN8LOF100036.spu"
  if (str_length(Site) > 5) {
    Site <- toupper(str_replace(filename,"(^.*?\\d{1,2})\\s*([a-zA-Z]*)(\\d{5,7}\\.spu$)","\\2"))
    # For 2012 and 2013 the spu filenames have ddmmmsite format; need to remove the 3 letter month.
    if (str_length(Site)> 5){
      pattern <- c("MAY","JUN","JUL", "AUG")
      for (i in 1:4){Site<- sub(pattern[i], "", Site)}
    }
  }
  FileNum <- as.integer(str_extract(filename, "[0-9]{5,7}"))
 
  # Extract info from the file itself, reading metadata from first 9 lines. Create a dataframe
  text <- read_lines(filename, n_max=9)
  DateTime <-  lubridate::mdy_hms(text[3], tz="America/Anchorage")
  Integration_ms <- as.numeric(strsplit(text[8], split = " ")[[1]][3])
  Temp <- as.numeric(strsplit(strsplit(text[5], split = " ")[[1]][4], split="=")[[1]][2])
  Remarks <- text[2]
  Spufilename_file <- tolower(str_replace(filename,".*[\\\\/]([A-z0-9.]\\s*)","\\1"))
  #Extract the file name in the spu file as a check. Some file names have spaces 
  Spufilename <- tolower(str_replace(text[1],".*[\\\\]([A-z0-9.]\\s*)","\\1")) 
  Spufilename <- str_replace(Spufilename,"\"","") # remove trailing quote
  metaData <- data_frame(Site=Site, FileNum=FileNum, DateTime=DateTime, Integration_ms=Integration_ms,
                         Temp=Temp, Remarks=Remarks, Spufilename=Spufilename )
  return(metaData)
}
read_spu_file_spectra <- function(filename) {
  
  # Read spectral intensity data into dataframe
  data <- read.table(file = filename, skip = 9, col.names = c("Wavelength", "ChB", "ChA")) %>% 
    mutate(Reflectance = ChB/ChA)
  # Only use the valid spectra
  data_valid <-  subset(data, Wavelength > 400 & Wavelength < 1000) 
  
  return(data_valid)
}

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@


# *********************************************************************************************
#  Go through each year and read in the information from both multispec processed files 
# and the raw spu files.
#**********************************************************************************************

# Get the Multispec process Excel files directory. >>>> NOTE: The dialog window may appear behind RStudio window
getwd() # Seems to help avoid errors in .jcall
proc_path <- rchoose.dir(caption = "Select Process files Directory")

# Get the list of filenames of processed directory (recursive=F) without path (full.names=F)
file_names <- list.files(proc_path, full.names= T, pattern='*.xlsx', recursive=FALSE)

# xlsx files - read key info from the processed data.
key_info <- map_dfr(file_names,function (x) read.processedfile (x))

#----------------------------------------------------------------------------------
#  Get just the Spectra sheet where the spu file names are not all in the "Spectra" sheet. true for 2012,2013
# if (any(is.na(key_info$Spufilename))){key_info <- map_dfr(file_names,function (x)read.process_spectrafile (x), .id = "process_file")
# }#----------------------------------------------------------------------------------

# Check for different spelling of site names
key_info$Site <- toupper(key_info$Site)
unique(key_info$Site)

# Recode them to stadard ones.
Site_Names <- list(Hist="HIST", HIS ="HIST", LOF = "LMAT", LoFert = "LMAT", 'Ex NP LF' = "LMAT",
                   LFMAT = "LMAT", NMAT = "MNAT",  MNT = "NANT", SHRB = "SHB")
key_info$Site <- recode (key_info$Site, !!!Site_Names, .default = key_info$Site)
unique(key_info$Site)

# check data for any na in  file numbers
check_df <- subset(key_info,is.na(key_info$FileNum))

# Add a key variable of form yyy-mm-dd_site_key number
key_info  <-  key_info %>%  mutate(key_num = ifelse (FileNum <999,  formatC(FileNum,width=5,flag="0"), FileNum),
                                   key = paste0(format(Date, format="%Y-%m-%d"), "_", Site, key_num))
# Save the data as a csv file in the form of yyyy_metadata_filekey.csv
write.csv(key_info, paste0(proc_path,"/",format(key_info$Date[1],format ="%Y"),"_metadata_filekey.csv"))

#***********************************************************************************************
# *******************Read .spu data*************************************************************

#  Choose and read the .spu files in the raw directory. 
getwd() # Seems to help avoid errors in .jcall
raw_data_path <- rchoose.dir(paste0(proc_path,"/.."),caption = "Select Raw data files Directory")
spu_files <- list.files(path = raw_data_path, pattern = ".spu$", full.names = T, recursive=T)

# Read all the metadata from the spu files using function read_spufile_metadata; add a variable with the filename
#  and max of ChA and ChB.  
spu_data <- map_dfr(spu_files,read.spufile.metadata) %>%
  mutate(Spu_filename=spu_files) %>%
  mutate(Spectra=map(Spu_filename, function(x) read_spu_file_spectra(x)))  %>%
  mutate(ReflecMean=map_dbl(Spectra, function(x) mean(x$Reflectance))) %>%
  mutate(ReflecSERR=map_dbl(Spectra, function(x) sd(x$Reflectance)/sqrt(length(x$Reflectance)))) %>%
  mutate(ChAmax= map_dbl(Spectra, function(x) max(x$ChA))) %>%
  mutate(ChBmax= map_dbl(Spectra, function(x) max(x$ChB))) %>%
  mutate(Number_ChAmax= map_dbl(Spectra, function(x) sum(x$ChA> 65350)))

# Check for different spelling of site names
unique(spu_data$Site)

# Recode them to stadard ones. This should cover must years.
Site_Names <- list(DH ="HTH", LHTH = "HTH", HTHB = "HTH", HTHPC = "HTH",HIS="HIST", 
                   LOF = "LMAT",  LOFB = "LMAT", LNB = "LMAT", LOFRB ="LMAT",
                   MATB="MAT", MATSL= "MAT", MATBK = "MAT", 
                   MANTB ="MNAT",MNATB ="MNAT", NAMTB = "MNAT", 
                   NMNT = "NANT", NANTB ="NANT", JULNB ="NANT",NMNTB ="NANT",
                   LSHB= "SHB", SHBB = "SHB", SHRBB = "SHB", SHRB = "SHB", 
                   LWSG = "WSG", WSGB = "WSG", WS ="WSG", WSB = "WSG", WSDB = "WSG")
spu_data$Site <- recode (spu_data$Site, !!!Site_Names, .default = spu_data$Site)
unique(spu_data$Site)

# Create a key variable that is unique for each spu file
spu_data <- spu_data %>%   mutate(key_num = ifelse (FileNum <999,  formatC(FileNum,width=5,flag="0"), FileNum),
                                  key = paste0(format(DateTime, format="%Y-%m-%d"), "_", Site, key_num))
spu_data_csv <- unnest(select(spu_data,-Spectra))
write.csv(spu_data_csv, paste0(raw_data_path,"/",format(spu_data$DateTime[1], format="%Y"),"_spu_metadata.csv"))

# Joint metadata to spu data and  use the Remarks variable to find the darkscan, and throw away scans and
# and use the Reflectance mean to find References scans. This will help check the metadata.  Not all spu files
# were used in the process files and some years inculde the used spu files in the the raw folder.
unispec_data <- full_join(key_info,spu_data, by = "key") %>%
  mutate( Treatment = ifelse(grepl("DARKscan",Remarks, fixed=T), "Darkscan", Treatment)) %>%
  mutate( Treatment = ifelse(grepl("Datascan,DC",Remarks, fixed=T), "Throwawayscan", Treatment)) %>%
  mutate(Type = if_else(ReflecMean>0.8,"Reference_scan","")) %>%   # Test for reference scan with a threshhold of 0.9
  mutate(Type = if_else(grepl("scan",Treatment),Treatment,
                        if_else(is.na(Treatment),"not_in_process_file",Type)))   # add darkscan and throwaway scans

# Save a csv file (minus the Spectra data list) and a R data file
save_filename <- paste0(raw_data_path,"/../",format(spu_data$DateTime[1], format="%Y"),"_unispecdata.csv")
write.csv(select(unispec_data,-Spectra),save_filename )
save(unispec_data,file = paste0(format(spu_data$DateTime[1], format="%Y"),"-unispec_data.Rda"))
#--------------------------------------------------------------------------------------------

