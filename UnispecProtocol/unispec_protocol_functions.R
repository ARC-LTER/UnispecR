# DESCRIPTION: R functions for processing newly collected unispec data
# AUTHOR: Ruby An
# DATE: 2019-03-05
# REVISED: 2019-06-17




# FUNCTIONS ---------------------------------------------------------------

read_spu_file_metadata <- function(filename) {
  
  ruby_year <- str_detect(filename, "2017|2018|2019")
  
  if (ruby_year) {
    ## Reads a .spu file from 2017 or 2018 
    
    # Filename metadata
    filename_metadata <- unlist(str_split(filename, pattern = "/")) %>% last() %>% str_split("_") %>% unlist()
    Site <- filename_metadata[2]
    Date <- filename_metadata[1]
    
    FileNum <- as.integer(str_extract(filename_metadata[3], "\\d{5}")) # extract 5 digits
    
    # Extract info from the file itself, reading metadata from first 9 lines. Create a dataframe
    text <- read_lines(filename, n_max=9)
    DateTime <-  lubridate::mdy_hms(text[3], tz="America/Anchorage")
    Date <- lubridate::date(DateTime)
    Integration_ms <- as.numeric(strsplit(text[8], split = " ")[[1]][3])
    Temp <- as.numeric(strsplit(strsplit(text[5], split = " ")[[1]][4], split="=")[[1]][2])
    Remarks <- text[2]
    
    # Truncated Filename 
    Spufilename <- unlist(str_split(filename, pattern = "/")) %>% last()
    
    # Metadata 
    metaData <- tibble(Site=Site, FileNum=FileNum, Date=Date, DateTime=DateTime, Integration_ms=Integration_ms,
                       Temp=Temp, Remarks=Remarks, spu_filename=Spufilename )
    
  } else {
    ## Reads a generic .spu file. Written for Historic (pre-2017) years. 
    
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
    
    # Avoid issues of digits in site/block name reading in as part of FileNum
    ## Based on the Unispec DC using 5 digits for automatic file numbering
    FileNum <- as.integer(str_replace(filename,"(^.*?\\d{1,2})(\\D*)(\\d{5})(\\.spu$)","\\3"))
    
    # Extract info from the file itself, reading metadata from first 9 lines. Create a dataframe
    text <- read_lines(filename, n_max=9)
    DateTime <-  lubridate::mdy_hms(text[3], tz="America/Anchorage")
    Date <- lubridate::date(DateTime)
    Integration_ms <- as.numeric(strsplit(text[8], split = " ")[[1]][3])
    Temp <- as.numeric(strsplit(strsplit(text[5], split = " ")[[1]][4], split="=")[[1]][2])
    Remarks <- text[2]
    
    #Extract the file name in the spu file as a check. Some file names have spaces 
    Spufilename <- tolower(str_replace(text[1],".*[\\\\]([A-z0-9.]\\s*)","\\1")) 
    Spufilename <- str_replace(Spufilename,"\"","") # remove trailing quote
    #Extract 
    Spufilename_file <- tolower(str_replace(filename,".*[\\\\/]([A-z0-9.]\\s*)","\\1"))
    
    
    
    metaData <- tibble(Site=Site, FileNum=FileNum, Date=Date, DateTime=DateTime, Integration_ms=Integration_ms,
                       Temp=Temp, Remarks=Remarks, spu_filename=Spufilename )
  }

  print(Spufilename) # use for error checking
  
  return(metaData)
}

read_spu_file_spectra <- function(filename) {
  # For a generic .spu file regardless of name
  
  # Read spectral intensity data into dataframe
  data <- read.table(file = filename, skip = 9, col.names = c("Wavelength", "ChB", "ChA")) %>% 
    mutate(Reflectance = ChB/ChA) 
  
  # Only use the valid spectra
  #  data_valid <-  subset(data, Wavelength > 400 & Wavelength < 1000) 
  
  return(data)
}

read_spu_file <- function(fileName) {
  # Written for 2017-2018 spu_files named with "SITE_FILENUM.spu" structure
  
  # Read metadata from first 9 lines of .spu file
  text <- read_lines(fileName, n_max = 9)
  
  # Get info from filename
  fileInfo <- str_split(text[1], pattern = c('\\\\'))[[1]]
  # works when files saved in default location on PCMIA card, not otherwise
  
  # Read spectral intensity data into dataframe
  data <- read.table(file = fileName, skip = 9, col.names = c("Wavelength", "ChB", "ChA"))
  
  # Tag with appropriate metadata
  data$Site <- str_extract(fileName, "(?<=/)([A-Z]{3,4}[0-9]*)(?=_)") # get string after last / & before _ 
  # for fileName format "DATE/SITE_FILENUM.spu", e.g. "04AUG2017/DHT_00000.spu"
  data$FileNum <- as.integer(str_extract(fileName, "[0-9]{5}"))
  data$Time <-  lubridate::mdy_hms(str_extract(text[3], "\\d+/\\d+/\\d{4}\\s\\d+:\\d{2}:\\d{2}\\s(PM|AM)"), tz="America/Anchorage")
  data$Date <- lubridate::date(data$Time)
  # lubridate::date(data$Time) # requires date/time is set correctly on Unispec DC 
  # lubridate::dmy(fileInfo[4]) # works when files saved in default location on PCMIA card
  data$int <- as.numeric(strsplit(text[8], split = " ")[[1]][3])
  data$Temp <- as.numeric(strsplit(strsplit(text[5], split = " ")[[1]][4], split="=")[[1]][2])
  
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
                        P1 = col_integer(),
                        P2 = col_integer(),
                        P3 = col_integer(),
                        P4 = col_integer(),
                        P5 = col_integer(),
                        Weather = col_character(),
                        Notes = col_character()
                      )) 
  
  # Consolidate measurements to tidy dataframe
  key_df <- key_csv %>% 
    gather(Replicate, FileNum, P1:P5) %>% 
    filter(!is.na(FileNum)) %>% 
    mutate(Replicate = str_extract(Replicate, "\\d+")) %>% 
    mutate(Date = lubridate::mdy(Date))
  
  return(key_df)
}

read_multispec_file <- function(multi_file) { #DIFFERENT NAMING CONVENTION THAN 2017
  ## Reads single multispec generated file
  ## Parses file name for usable info, format: YEAR-MONTH-DAY_SITE_{BLOCKS_FILENUMS}_TYPE.csv
  ## Reads in metadata from first 5 lines
  ## Reads in reflectance data (400-1100nm Wavelengths) from following lines 
  
  # to check for erros
  print(multi_file) 
  
  ## Exctract Metadata
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
    nest(Wavelength, Reflectance, .key = Spectra) %>% 
    mutate(Date = ymd(Date))
  
  tidy_df$spu_filename <- str_c(date, spu_files, sep = "/")
  if(type == "correct") {
    tidy_df$ref_files <- str_c(unique(ref_files[ref_files != ""]), collapse = ", ")
  } else {
    tidy_df$ref_files <- NA
  }
  
  return(tidy_df)
}

read_xlsx_file <- function(filename) {
  # Read files that were processed by Multispec and saved as excel file
  #   These files have two sheets: "Notes" and "Spectra".
  #   Notes: metadata on weather, reference files, time of measurements
  #   Spectra: multispec reflecatnce, reflectance file names/numbers, site, treatment and block info
  
  print(filename)  # Uncomment for testing which file is trouble.
  
  ## Find the "Wavelength" row (different for 2015)
  wavelength_row  <- read.xlsx(xlsxFile = filename, sheet = "Spectra", skipEmptyRows = F, cols = 1, rows = seq(1,50), colNames = F) %>% 
    pull() %>% grep(pattern = "Wavelength")
  
  ## Read the first set of rows of the "Spectra" sheet to get the metadata for the spu files
  
  df_metadata <- read.xlsx(xlsxFile = filename, sheet = "Spectra", skipEmptyRows = T, rows = 1:wavelength_row, detectDates = F) %>%
    set_tidy_names(.) %>%
    gather(columX,valname,-Reflectance) %>%
    spread(Reflectance,valname)%>%                # Transposed the first row to column variables
    dplyr::rename(spu_filename = Wavelength, Research_location = SITE, Site = EXPERIMENT, Date = DATE, Block = BLOCK, 
                  Time = TIME, Treatment = TREATMENT, Replicate = REP) %>%  # Rename the columns
    mutate(xlsx_filename = str_replace(filename,".*[\\\\/]([A-z0-9.]\\s*)","\\1")) %>% # Add processed filename
    select (-YEAR, -columX) %>%
    select_all(~gsub("\\s+|\\.", "", .)) %>%        # remove spaces from column names
    select( Research_location, Date, Time, Block, Site, Treatment, Replicate, spu_filename, xlsx_filename, 'NDVI(MODIS)',
            'EVI(MODIS)', 'EVI2(MODIS)','PRI(550Reference)', 'PRI(570Ref)',
            'WBI','ChlIndex')  %>% 
    # set column classes
    #mutate_at(.funs = funs(factor), .vars = vars("Research_location", "Site", "Block", "Treatment")) %>% 
    mutate_at(.funs = funs(as.numeric), .vars =  c('NDVI(MODIS)',
                                                   'EVI(MODIS)', 'EVI2(MODIS)','PRI(550Reference)', 'PRI(570Ref)',
                                                   'WBI','ChlIndex')) 
  
  ## Read the rows of the "Spectra" sheet to get the processed spectral data for the spu files
  df_spectra <- read.xlsx(xlsxFile = filename, sheet = "Spectra", skipEmptyRows = T, rows = wavelength_row:(wavelength_row+821), detectDates = F) %>%
    set_tidy_names(.) %>%
    gather(spu_filename,Reflectance,-Wavelength) %>%
    group_by(spu_filename) %>% 
    nest(.key = multispec_spectra) %>% 
    inner_join(df_metadata) %>% # join w/metadata by spu_filename
    mutate(Site = toupper(Site))
  
  # Read first 30 rows of Notes sheet to get the metadata from a processed file
  df_references <- read.xlsx(xlsxFile = filename, sheet = "Notes", skipEmptyRows = T,rows = 1:30) %>%
    gather(columnx,valname, -Summary.Information) %>%
    spread(Summary.Information,valname) %>%        # Transposed the first row to column variables
    dplyr::rename(Research_location = SITE, Date = 'Date Collected',spu_filename ='White Panel Reflectance File Used',   #rename them to match df_specra 
                  Site = 'LTER Experiment',Experiment = EXPERIMENT, Block = BLOCK, 
                  Weather = 'Brief Weather Description', Time = 'Approximate Time of Collection')  %>%
    mutate(Site = ifelse(is.na(Site),str_replace(filename,".*[/](\\w*)[_]+.*$","\\1"), Site)) %>%
    arrange(Date) %>%  # If there are more then 9 columns of reference files then need to make sure the Date value is first 
    mutate(Research_location = Research_location[1], Date = Date[1], Time = Time[1], Site = Site[1], Treatment = paste0("REF",row.names(.))) %>%
    mutate(xlsx_filename = str_replace(filename,".*[\\\\/]([A-z0-9.]\\s*)","\\1")) %>% # add processed file name
    select(Research_location, Date, Time, Site, Block, Treatment, spu_filename, xlsx_filename, Weather) %>%
    mutate(spu_filename = str_replace(spu_filename,"^.*[:*](.*$)","\\1")) %>%
    unnest(spu_filename = strsplit(spu_filename, "\\s*,\\s*")) %>%  # spu_filenames can be a comma delimited string of files; create a row for each
    mutate(spu_filename =ifelse(str_detect(spu_filename,".spu"), spu_filename, paste0(tolower(spu_filename),".spu"))) 
  
  # Create reference key per summary sheet
  df_ref_key <- df_references %>%
    group_by(Research_location, Date, Site, xlsx_filename) %>% 
    summarize(ref_filenames = str_c(spu_filename, collapse = ", "))  %>% 
    ungroup()
  
  # Add ref_filenames column & Append the references spectra to the measured spectra
  df_all <- inner_join(df_ref_key,df_spectra) %>% # add ref_filenames column 
    full_join(df_references) # add references
  
  df_all <- df_all %>%
    mutate(spu_filename = tolower(trimws(spu_filename)),
           FileNum = as.integer(str_replace(spu_filename,"(^.*?\\d{1,2})(\\D*)(\\d{5})(\\.spu$)","\\3")),
           Date = convertToDate(Date,tz ="America/Anchorage")) # convert excel date to universal date 
  
  
  return(df_all)  
}


interpolate_spectra <- function(spectra) {
  # interpolates unispec spectra to 1nm 
  # input: dataframe with Wavelength & Reflectance columns
  # output: dataframe with Wavelength (every 1 nm) & Reflectance columns 
  #spectra <- spectra[[1]]
  
  is.null(spectra)
  interp <- approx(x = spectra$Wavelength, y = spectra$Reflectance, 
                   xout = seq(400,1000, by = 1), method = "linear")
  spectra_interp <- tibble(Wavelength = interp$x, Reflectance = interp$y)
  
  return(spectra_interp)
}

check_same_references <- function(m, r) {
  # DESCRIPTION: Compares two strings to see if multispec and rscript use same white reference files
  # Input: character strings "multispec_ref_filenames" and "rscript_ref_filenames"
  # Output: Logical value
  
  m <- str_split(m, pattern = ", ") %>% unlist() %>% str_squish() %>% sort()
  r <- str_split(r, pattern = ", ") %>% unlist() %>% str_squish() %>% sort()
  
  if(length(m) == length(r)) {
    return(all(m == r))
  } else {
    return(F)
  }

}






calculate_spectral_bands <- function(spectra, band_defns, instruments = c("MODIS")) {
  # Calculates spectral bands from dataframe including Wavelength & Reflectance
  ## inputs: spectra - Wavelength, Reflectance columns, requires interpolation to 1nm
  ##         band_defns : wavelengths definining colors 
  ##         instrument : e.g. MODIS, SKYE, ITEX
  ##         bands   : the spectral bands to return, e.g. red, blue, nir, etc. 
  ## output: spectra_bands = dataframe with Definition, Band, Averaged Reflectance
  
  
  bands <- band_defns %>% 
    filter(definition %in% instruments)
  
  # vector of wavelengths, one set per instrument
  wavelengths <- seq(300, 1200, by = 1)
  
  # dataframe of wavelengths labeled by instrument & color
  bands_df <- tibble(Wavelength = rep(wavelengths, times = length(instruments)), 
                     definition = rep(instruments, each = length(wavelengths))) %>% 
    full_join(bands) %>% 
    mutate(color_match = ifelse(Wavelength >= min & Wavelength <= max, color, NA)) %>% 
    select(Wavelength, definition, color_match) %>% 
    distinct()
  
  # interpolated spectra to 1nm 
  interp <- approx(x = spectra$Wavelength, y = spectra$Reflectance, 
                   xout = seq(300,1200, by = 1), method = "linear")
  spectra_interp <- tibble(Wavelength = interp$x, Reflectance = interp$y)
  
  ## DATA: join to interpolated spectra 
  spectra_bands <- full_join(spectra_interp, bands_df) %>% 
    group_by(definition, color_match) %>% 
    summarize(average_reflectance = mean(Reflectance)) %>% 
    filter(!is.na(color_match)) %>% 
    rename(band = color_match)
  
  return(spectra_bands) 
}

# # Calcualte individual color bands
# spectral_band_data <- spu_data %>% mutate(Bands = map(Spectra, function(x) calculate_spectral_bands(x, band_defns = band_defns, instrument = c("MODIS", "ITEX", "ToolikEDC"))))
# 
# # Calculate indices from color bands
# spectral_band_data %>% unnest(Bands) %>% # unnest the "Band" tibble to individual rows
#   spread(key = band, value = average_reflectance) %>% # spread the Bands into their own columns
#   mutate(NDVI = (nir-red)/(nir+red), # calculate vegetation indices of your choice as new columns
#          EVI = 2.5*((nir-red)/(nir+6*red-7.5*blue + 1)),
#          EVI2 = 2.5*((nir-red)/(nir+2.4*red + 1))) %>% 
#   select(spu_filename, definition, NDVI) # select relevant columns 



calculate_indices <- function(spectra, band_defns, instrument = "MODIS", indices = "NDVI") {
  # Calculates NDVI, EVI, and EVI2 from dataframe including Wavelength : Spectra 
  ## inputs: spectra - Wavelength, Reflectance columns
  ##         band_defns : wavelengths definining colors 
  ##         instrument : e.g. MODIS, SKYE, ITEX
  ##         indicies   : the index to return 
  ## output: dataframe with Index : Value
  
  bands <- band_defns %>% 
    filter(definition == instrument) 
  
  blue <- bands %>% filter(color=="blue") %>% select(min, max) %>% as.numeric()
  nir <- bands %>% filter(color=="nir") %>% select(min, max) %>% as.numeric()
  red <- bands %>% filter(color=="red") %>% select(min, max) %>% as.numeric()
  
  spectra_bands <- spectra %>% 
    mutate(color = ifelse(Wavelength >= blue[1] & Wavelength <= blue[2], "blue",
                          ifelse(Wavelength >= red[1] & Wavelength <= red[2], "red",
                                 ifelse(Wavelength >= nir[1] & Wavelength <= nir[2], "nir",
                                        "other")))) %>% 
    group_by(color) %>% 
    summarize(Reflectance = mean(Reflectance))
  
  index_data <- spectra_bands %>%
    spread(color, Reflectance) %>% 
    mutate(NDVI = (nir-red)/(nir+red),
           EVI = 2.5*((nir-red)/(nir+6*red-7.5*blue + 1)),
           EVI2 = 2.5*((nir-red)/(nir+2.4*red + 1))) %>% 
    select_at(indices) %>% 
    gather(Index, Value, everything())
  
  return(index_data) 
}

plot_spectra <- function(df_subset) {
  # Plot spectra from a subset of a dataframe
  
  plot_check <- df_subset %>%
    unnest(Spectra) %>%
    filter(Wavelength > 400, Wavelength < 1000) %>% 
    gather(key = Channel, value = Intensity, ChB, ChA) %>%
    gather(key = ref_part, value = Reflectance_Intensity, Intensity, Reflectance)
  
  
  ## Plot Specified Correction Factors for quality check
  plot_zoom <- ggplot(data = plot_check, mapping = aes(x = Wavelength, y = Reflectance_Intensity)) +
    geom_line(aes(color=Channel)) +
    facet_grid(ref_part ~ spu_filename, scales = "free")
  # 
  # if("Treatment" %in% names(df_subset)) { # use for datafarmes with and without metadata
  #   plot_zoom <- plot_zoom + 
  #     facet_grid(ref_part ~ DateTime + Site + FileNum + Treatment, scales="free")
  #   
  # } else {
  #   plot_zoom <- plot_zoom + 
  #     facet_grid(ref_part ~ DateTime + Site + FileNum, scales="free")
  #   
  # }
  
  return(plot_zoom)
}


plot_channels <- function(df_subset) {
  # Plot spectra from a subset of a dataframe
  
  plot_check <- df_subset %>%
    unnest(Spectra) %>%
    filter(Wavelength > 400, Wavelength < 1000) %>% 
    gather(key = Channel, value = Intensity, ChB, ChA) %>%
    gather(key = ref_part, value = Reflectance_Intensity, Intensity, Reflectance)
  
  
  ## Plot Specified Correction Factors for quality check
  plot_zoom <- ggplot(data = plot_check, mapping = aes(x = Wavelength, y = Reflectance_Intensity)) +
    geom_line(aes(color=Channel)) 
  
  if("Treatment" %in% names(df_subset)) { # use for datafarmes with and without metadata
    plot_zoom <- plot_zoom + 
      facet_grid(ref_part ~ DateTime + Site + FileNum + Treatment, scales="free")
    
  } else {
    plot_zoom <- plot_zoom + 
      facet_grid(ref_part ~ DateTime + Site + FileNum, scales="free")
    
  }
    
  return(plot_zoom)
}

plot_reflectances <- function(df_subset) {
  raw_spectra <- df_subset %>% 
    unnest(Spectra) %>%
    filter(Wavelength > 400, Wavelength < 1000) %>% 
    mutate(Status = "raw")
  
  xlsx_spectra <- df_subset %>% 
    filter(!is.na(ref_filenames)) %>% 
    unnest(multispec_spectra) %>%
    filter(Wavelength > 400, Wavelength < 1000) %>% 
    mutate(Status = "multispec")
  
  spectra <- bind_rows(raw_spectra, xlsx_spectra)
  
  plot_zoom <- ggplot(data = spectra, mapping = aes(x = Wavelength, y = Reflectance, color = Status)) +
    geom_line() + 
    facet_wrap(vars(Date, Site, Block, Treatment, FileNum))
  
  return(plot_zoom)
}

check_time_difference <- function(df_check) {
  
  if(!("DateTime" %in% names(df_check))) {
    return(print("no DateTime"))
  } else {}
  
  # Select columns to Check
  timedata <- df_check %>% 
    select_if(function(x) typeof(x) != "list") %>% 
    group_by(DateTime) %>% arrange(DateTime) %>% distinct() 
  
  # Calculate time difference between measurements
  timedata$diff <- timedata$DateTime - lag(timedata$DateTime)
  
  # Check time differences
  time_check <- timedata %>% select(DateTime, spu_filename, Site, Block, Treatment, Replicate, FileNum, diff, everything()) #%>% filter(FileNum>=0 & FileNum <=15)
  
  return(time_check)
}

