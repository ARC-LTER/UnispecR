# R functions for Unispec Data Processing Procedure
# AUTHOR: Ruby An
# DATE: 2018-06-24
# REVISED: 2018-11-29

# FUNCTIONS ---------------------------------------------------------------

read_spu_file <- function(fileName) {
  # Read metadata from first 9 lines of .spu file
  text <- read_lines(fileName, n_max = 9)
  
  # Get info from filename
  fileInfo <- str_split(text[1], pattern = c('\\\\'))[[1]]
  # works when files saved in default location on PCMIA card, not otherwise
  
  # Read spectral intensity data into dataframe
  data <- read.table(file = fileName, skip = 9, col.names = c("Wavelength", "ChB", "ChA"))
  
  # Tag with appropriate metadata
  data$Site <- str_extract(fileName, "(?<=/)([^/]+)(?=_)") # get string after last / & before _
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
                        P5 = col_integer()
                      )) 
  
  # Consolidate measurements to tidy dataframe
  key_df <- key_csv %>% 
    gather(Measurement, FileNum, P1:P5) %>% 
    filter(!is.na(FileNum)) %>% 
    mutate(Measurement = str_extract(Measurement, "\\d+")) %>% 
    mutate(Date = lubridate::mdy(Date))
  
  return(key_df)
}

calculate_ndvi <- function(tidydata, nir = c(820,890), red = c(640, 680)) {

  red_data <- tidydata %>%
    filter(Wavelength >= red[1] & Wavelength <= red[2]) %>%
    group_by(Site, Block, Treatment, Date, Measurement, Type) %>%
    summarise(
      red = mean(Reflectance)
    )

  nir_data <- tidydata %>%
    filter(Wavelength >= nir[1] & Wavelength <= nir[2]) %>%
    group_by(Site, Block, Treatment, Date, Measurement,Type) %>%
    summarise(
      nir = mean(Reflectance)
    )

  ndvi_data <- inner_join(nir_data, red_data) %>%
    mutate(ndvi = (nir-red)/(red+nir))

  return(ndvi_data)
}

calculate_index <- function(tidydata, band_defn ="MODIS", indices = c("NDVI", "EVI", "PBI_550", "PRI_570", "WBI", "Chl")) {
  # Default MODIS bands for Red, NIR, Blue

  if (band_defn == "ITEX") {
    red <- c(560, 680)
    nir <- c(725, 1000)
  } else if (band_defn == "MODIS") {

    red <- c(620, 670)
    nir <- c(841, 876)
    blue <- c(459, 479)
  } else if (band_defn == "SKYE" ) {
    red <- c(620, 680)
    nir <- c(830, 880)
    blue <- c(455, 480)
  } else if (band_defn == "Toolik-GIS-drone") {
    # 2018 values
    red <- c(640,680)
    nir <- c(820,890)

  } else {
    print("ERROR - specify band definition")
  }

  ## Calculate NDVI, EVI, EVI2
  index1_data <- tidydata %>%
    mutate(color = ifelse(Wavelength >= blue[1] & Wavelength <= blue[2], "blue",
                          ifelse(Wavelength >= red[1] & Wavelength <= red[2], "red",
                                 ifelse(Wavelength >= nir[1] & Wavelength <= nir[2], "nir",
                                        "other")))) %>%
    group_by(Site, Block, Treatment, Date, Measurement, color) %>%
    summarize(Reflectance = mean(Reflectance)) %>%
    spread(color, Reflectance) %>%
    mutate(NDVI = (nir-red)/(nir+red),
           EVI = 2.5*((nir-red)/(nir+6*red-7.5*blue+1)),
           EVI2 = 2.5*((nir-red)/(nir+2.4*red + 1)))

  ## Calculate PRI, WBI, Chl
  index2_data <- tidydata %>%
    group_by(Site, Block, Treatment, Date, Measurement, FileNum) %>%
    summarize(PRI_550 = (Reflectance[Wavelength == 550][1] - Reflectance[Wavelength == 531][1])/
                (Reflectance[Wavelength == 550][1] + Reflectance[Wavelength == 531][1]),
              PRI_570 = (Reflectance[Wavelength == 570][1] - Reflectance[Wavelength == 531][1])/
                (Reflectance[Wavelength == 570][1] + Reflectance[Wavelength == 531][1]),
              WBI = Reflectance[Wavelength == 900][1] / Reflectance[Wavelength == 970][1],
              Chl = (Reflectance[Wavelength == 750][1] - Reflectance[Wavelength == 705][1])/
                (Reflectance[Wavelength == 750][1] + Reflectance[Wavelength == 705][1]))


  index_data <- inner_join(index1_data, index2_data) %>%
    select(-c(red, blue, nir, other)) %>%
    select(indices)

  return(index_data)
}
