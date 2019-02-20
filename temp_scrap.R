
# Shiny version of loading keys -------------------------------------------

## Load Keys 
Select unispec key files interactively or search within: `r data_path`. 

```{r key, echo=F} 
fileInput("key", "Choose unispec key CSV File",
          multiple = TRUE,
          accept = c("text/csv",
                     "text/comma-separated-values,text/plain",
                     ".csv"))
tableOutput("keys")
```

```{r key_load, context="server"}
output$keys <- renderTable({
  key_list <- tibble(keyname = input$key) %>% # create dataframe
    mutate(key_contents = map(keyname, function(x) read_key_file(x))) 
  
  ## Unpack into usable dataframe 
  keys <- unnest(key_list)
  
  keys
})

```










## Check White References
White references correct for instrument & cable irregularities. Multiplying by the correction factor (ChA/ChB) smooths out the spectra. If multiple file numbers are listed (typically 5), the correction factors are averaged. 

### Plot All References
The following code chunk plots all the white reference spectra in your chosen directory.

```{r refs, echo = F, warning=F}

## Find all white reference data files 
ref_data_all <- df_clean %>% 
  filter(str_detect(Treatment, "REF")) %>% # extract reference data 
  ## The following steps expand the "Block" column tocreate one REF set per Block per Site -- as in the keys above.
  separate(Block, into = c("BX1", "BX2", "BX3", "BX4"), sep = ",") %>% #1
  gather(Block, BlockString, BX1:BX4) %>% #2
  mutate(Block = str_squish(BlockString), BlockString=NULL) %>% #3
  filter(!is.na(Block)) %>% #4
  mutate(CorrectionFactor_REF = ChA/ChB) # calculate correction fact column

## Select a subset to plot 
ref_dates <- ref_data_all %>% select(Date) %>% 
  unique() %>% 
  slice(1:n()) %>% 
  c() # list of dates present in data 

ref_date_subset  <- ref_dates$Date

## Plot Correction Factors for quality check 
ref_data_plot <- ref_data_all %>% 
  filter(Date %in% ref_date_subset) %>% 
  gather(key = Channel, value = Intensity, ChB, ChA, CorrectionFactor_REF) %>% 
  mutate(Channel = factor(Channel))

cor_factor_plot_all <- ggplot(data = ref_data_plot, mapping = aes(x = Wavelength, y = Intensity)) +
  geom_line(aes(color=Measurement, linetype=Block, alpha=Treatment)) +
  facet_grid(Channel ~ Date + Site, scales="free") + 
  scale_alpha_discrete(range=c(1, 0.5)) # set transparency by block

cor_factor_plot_all

```


Look for correction factors very far from 1.0 or with odd peaks. 


### Plot Zoom Check
Run the following code chunk interactively in RStudio to check references at specific sites/dates. 
```{r check_refs}
# Useful Vectors for filtering
sites <- unlist(site_list)
dates <- df_clean %>% select(Date) %>% unique() %>% slice(1:n()) %>% c() # list of dates present in data

## SPECIFY SITE/DATE/ETC to ZOOM IN ON
check_sites <- c("MAT")
check_dates <- dates[[1]][3] # necessary to unlist dates vector

ref_check <- df_clean %>% # full dataframe not just ref's 
  filter(Date %in% check_dates) %>% 
  filter(Site %in% check_sites) %>% 
  filter(FileNum %in% c(2,3,4)) %>% # which files 
  mutate(CorrectionFactor_REF = ChA/ChB)

## Plot Specified Correction Factors for quality check 
cor_factor_plot_zoom <- ggplot(data = ref_check, mapping = aes(x = Wavelength, y = CorrectionFactor_REF)) +
  geom_line(aes(color=Treatment, linetype=Measurement, alpha=Block)) +
  facet_grid(Site ~ FileNum) +
  scale_alpha_discrete(range=c(1, 0.25))

cor_factor_plot_zoom
```

Look at associated dataframe to check info, specifically: *Weather*, *Notes*, or *int* (Integration Time) columns.
```{r ref_check_files, echo=F}

## EXAMINE SPECIFIC info: check Integration, Notes, Weather, filename, etc. 
ref_check_files <- ref_check %>% 
  select(-c(Wavelength, ChA, ChB, CorrectionFactor_REF, keyname, Time)) %>% 
  unique()

### View dataframe of ref_check_files
kable(ref_check_files)
```

## Choose White References:
Based on the above quality check, choose reference files by entering the appropriate file numbers in **`r key_files`** for the rows where the column *Treatment* = **REF**. There are typically 5 reference measurements per *Date* / *Site*. 

Then rerun the **Load Keys** and **Join Data & keys** sections above to update the `df` dataframe. The following plots your chosen references.

```{r ref_table, echo=F}
options(knitr.kable.NA = '')

## Build REF key 
ref_keys <- keys %>% 
  filter(Treatment == "REF") %>% # extract reference data 
  ## The following separates the Site column into "Site" and "Site_REF"
  separate(Site, into=c("Site", "Site_REF"), sep = "_", fill="right") %>% 
  mutate(Site_REF = coalesce(Site_REF, Site)) 


## Find REF files for correction factors
ref_data <- data %>% 
  rename(Site_REF = Site) %>% 
  inner_join(ref_keys) %>% 
  mutate(CorrectionFactor_REF = ChA/ChB) 


## Plot CHOSEN Correction Factors 
cor_factor_plot  <- ggplot(data = ref_data, mapping = aes(x = Wavelength, y = CorrectionFactor_REF)) +
  geom_line(aes(color=Measurement, linetype=factor(int))) +
  facet_wrap(vars(Date,Site_REF, Block), ncol=4)

cor_factor_plot


## Output Table of Chosen References
ref_choice <- ref_data %>% 
  select(Date, Site, Site_REF, FileNum, Block, Measurement, Weather, Notes, filename) %>% 
  distinct() %>% 
  mutate(Measurement = str_c("P", Measurement))%>% 
  spread(Measurement, FileNum) %>% 
  unite(FileNums, P1:P5, sep=",") 

kable(ref_choice, caption="White Reference Files")

```

## Apply References by Key
Apply your chosen references to actual spectral data to create the tidy dataframe `df_tidy` containing corrected sepectral reflectance values. 

```{r apply_refs, echo=FALSE}
## Average 5 chosen ref measurements per DATE/SITE/BLOCK 
ref_summary <- ref_data %>% 
  ## The following steps expand the "Block" column to create one REF set per Block per Site. This structure is necessary for situtations where different refs are used for different blocks at the same site. 
  separate(Block, into = c("BX1", "BX2", "BX3", "BX4"), sep = ",") %>% #1: expand string entry in "Block" into separate columns
  gather(Block, BlockString, BX1:BX4) %>% #2: re-condense into one column, generates correct number of rows per site AND per block
  mutate(Block = str_squish(BlockString), BlockString=NULL) %>% #3: replace placeholder column names w/"B1-B4". Also removes whitespace from BlockString contents introduced by "separate" function
  filter(!is.na(Block)) %>% #4: remove empty rows for sites w/out B3 or B4
  ### The following code group repeated REF measurements, and takes the mean 
  group_by(Date,Site,Block,Wavelength) %>% 
  # group_by(Date,Site,Block,Wavelength, int) %>% # to separate integration times
  summarize(ChA_REF = mean(ChA), ChB_REF = mean(ChB), CorrectionFactor_REF = mean(ChA/ChB), int_REF = mean(int), Notes_REF = str_c(Notes, collapse = "; "), FileNames_REF = str_c(filename,collapse = ", "))

## Join DATA with REFS 
df_ref <- inner_join(df_clean, ref_summary) %>% 
  select(Date, Time, Site, Block, Treatment, Measurement, Wavelength,  int, int_REF, ChB, ChA, ChB_REF, ChA_REF, CorrectionFactor_REF, Weather, Notes, Notes_REF, FileNames_REF, filename, FileNum, keyname) %>%
  mutate(raw = ChB/ChA) %>% # the raw reflectance
  mutate(correct = raw*CorrectionFactor_REF) %>% # this step calculates the corrected reflectance
  gather(Type, Reflectance, raw:correct) 

df_tidy <- df_ref %>% 
  mutate(Site = replace(Site, Site %in% WSG, "WSG")) %>% # rename WSG1 & WSG23 to WSG
  mutate(Site = replace(Site, Site %in% SHB, "SHB"))# %>%  # rename  SHB1 & SHB2 to SHB
# filter(Type == "correct") %>% # drop raw reflectance
# select(-Type) 
```


# Pulling vectors from dataframe ------------------------------------------

# Useful Vectors for filtering
sites <- df_clean %>% select(Site) %>% unique() %>% pull()
dates <- df_clean %>% select(Date) %>% unique() %>% pull() # list of dates present in data


