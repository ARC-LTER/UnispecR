## Code to read in & process red/far-red unispec-dc data
## Ned Fetcher // Ruby An
## 2019-08-06 to 2019-08-07



# Required Packages  ------------------------------------------------------

require("tidyverse")


# Functions ---------------------------------------------------------------


read_spu_file_metadata_rfr <- function(filename) {

  ## Reads a .spu file from 2017 or 2018 
  
  # Filename metadata
  filename_metadata <- unlist(str_split(filename, pattern = "/")) %>% tail(3)
  Date <- filename_metadata[1]
  When <- filename_metadata[2]
                                                                     
  prefix_num <- filename_metadata[3] %>% str_split("_") %>% unlist()
  Site <- prefix_num[1]
  
  FileNum <- as.integer(str_extract(prefix_num[2], "\\d{5}")) # extract 5 digits
  
  # Extract info from the file itself, reading metadata from first 9 lines. Create a dataframe
  text <- read_lines(filename, n_max=9)
  DateTime <-  lubridate::mdy_hms(text[3])
  Date <- lubridate::date(DateTime)
  Integration_ms <- as.numeric(strsplit(text[8], split = " ")[[1]][3])
  Temp <- as.numeric(strsplit(strsplit(text[5], split = " ")[[1]][4], split="=")[[1]][2])
  Remarks <- text[2]
  
  # Truncated Filename 
  Spufilename <- unlist(str_split(filename, pattern = "/")) %>% last()
  
  # Metadata 
  metaData <- tibble(Site=Site, FileNum=FileNum, Date=Date, DateTime=DateTime, Integration_ms=Integration_ms,
                     Temp=Temp, Remarks=Remarks, spu_filename=Spufilename )
    
 
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



# Read in Files  ----------------------------------------------------------
## Read .spu raw files
data_path <-  "/Users/toolik/OneDrive - Marine Biological Laboratory/Toolik Terrestrial/UnispecData/2019/"

raw_data_path <- paste0(data_path, "n.fetcher")
spu_files <- list.files(path = raw_data_path, pattern = ".spu$", full.names = T, recursive=T)

# Read all spu_files 
spu_metadata <- map_dfr(spu_files, read_spu_file_metadata_rfr) %>% 
  mutate(spu_filename_full = spu_files)

spu_data <- spu_metadata %>% 
  mutate(Spectra=map(spu_filename_full, function(x) read_spu_file_spectra(x)))


plot_channels(spu_data %>% slice(1:5))
