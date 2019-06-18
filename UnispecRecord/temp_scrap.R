
# process_2017_unispec_files ----------------------------------------------


## Output Table of Chosen References
ref_choice <- ref_data %>% 
  select(Date, Site, Site_REF, FileNum, Block, Replicate, Weather, Notes, spu_filename) %>% 
  distinct() %>% 
  mutate(Replicate = str_c("P", Replicate))%>% 
  spread(Replicate, FileNum) %>% 
  unite(FileNums, P1:P5, sep=",") 

kable(ref_choice, caption="White Reference Files")


## Join all data matching keys dataframe (drops any non-matching data),
##  by columns (Date, Site, FileNum)
df <- inner_join(data, keys)  

# read and clean unispec record 2017-2018 ---------------------------------




df$Site <- recode (df$Site, !!!Site_Names, .default = df$Site)
unique(df$Site)

load("UnispecData/multispec_data_2017.Rda")

multispec_data <- multispec_data_2017 %>% 
  rename(Replicate = Measurement) %>% 
  rename(ProcessType = Type) %>% 
  mutate(Replicate = as.integer(Replicate)) %>% 
  nest(Wavelength, Reflectance, .key = "multispec_spectra")


# Standardize site names
#unispec_key$Site <- recode (unispec_key$Site, !!!Site_Names, .default = unispec_key$Site)

# Add info to join to raw spu_data (Site, Date, FileNum)
spu_data_plus <- spu_data %>% mutate(Date = lubridate::date(DateTime)) %>% 
  mutate(Site =  str_replace(spu_filename, ".*[_](.*)[_]+.*$","\\1")) %>% # get string after / & before _
  mutate(FileNum =  as.integer(str_extract(spu_filename, "[0-9]{5}")))

# read and clean unispec record -------------------------------------------

### Save clean dataframe for Natalie Kashi
```{r}

unispec_dataframe <- read_rds("UnispecData/2015_unispec_problem_dataframe.rds")

kashi_data <- unispec_dataframe %>% 
  filter(Treatment %in% c("N", "P", "NP", "CT", "CT1", "CT2",
                          "F0.5", "F1", "F2", "F5", "F10", "NO3", "NH4")) %>% 
  filter(Site %in% c("LMAT", "NANT", "MNAT")) %>% 
  mutate(mislabeled = key_fix, file_problem = any(maxed, dim, absurd_reflectance)) %>% 
  rename(raw_spectra = Spectra, corrected_spectra = multispec_spectra) %>% 
  select(spu_filename, FileNum, Date, Site, Block, Treatment, Replicate, ref_filenames, ref_problems, file_problem,mislabeled, maxed:absurd_reflectance, raw_spectra, corrected_spectra) %>% 
  mutate_at(.funs = factor, .vars = c("Site", "Block", "Treatment", "Replicate", "mislabeled" ))

kashi_data %>% select_if(function(x) typeof(x) != "list") %>% summary()

kashi_data %>% unnest(corrected_spectra)

write_rds(kashi_data, "UnispecData/2015_unispec_data_LMAT-MNAT-NANT_kashi.rds")
                                         
                                

```

check_file_for_problems <- function(file, problem_files) {
  #INPUTS
  ## files = vector of spu_filenames
  ## problem_files = list of problematic files (i.e. maxed, absurd, mislabeled_refs, etc.)
  
  #OUTPUT
  # string vector cocatenated by ", " of problem types
  
  if(is.na(file)) { # check for NA file
    return(NA)
  } else { # search for file in list of problem files
    problem_index <- map(problem_files, function(x) grep(file, x))
    
    problems <- problem_index[lapply(problem_index, length) > 0] %>% names() %>% str_c(collapse = ", ")
    if (length(problems) == 0) {
      return(NA)
    } else {
      return(problems)
    }
  }
  
}

# Review Dates & Sites
```{r list_table, dependson="df"}
## Sites per Date table to display
df_table <- df %>%
  select(Date, Site) %>%
  distinct() %>%
  group_by(Date) %>%
  do(Sites = t(.[-1])) %>%
  mutate(Sites = paste( unlist(Sites), collapse=','))

kable(df_table)

## List date, Site, Treatment, Number of files 

df %>% group_by(Date) %>% 
  mutate_at(vars(Site), funs(as.character)) %>% 
  summarize(Sites = str_c(unique(Site), collapse = ", "), Files = n_distinct(spu_filename)) %>% kable()


```

## FILE LISTS

### Missing REF files (no raw spu data)
missing_ref_data <- xlsx_key %>% 
  filter(str_detect(Treatment, "REF")) %>%
  anti_join(spu_spectra)

missing_ref_files <- missing_ref_data$spu_filename

### Mislabeled / Absurd REF files 
mislabeled_ref_files <- unispec_key_fix %>% 
  filter(str_detect(Notes, "Mislabeled as REF")) %>% # from Unispec Key Fix
  pull(spu_filename) %>% 
  c(ref_data_mistakes$spu_filename) %>% # from absurd Correction Factor
  unique()

### Maxed REF files
maxed_ref_files <-  ref_files[ref_files %in% maxed_files_bad]
----------------------------------
calculate_spectra_stats <- function(Spectra) {
  
  spectra_stats <- tibble(ChA_min = min(Spectra$ChA), 
                          ChA_max = max(Spectra$ChA), 
                          ChB_max = max(Spectra$ChB),
                          num_maxed = max(sum(Spectra$ChA > 65000), sum(Spectra$ChB > 65000)),
                          Reflectance_mean = mean(Spectra$Reflectance))
  
  return(spectra_stats)
}

### Calculate Spectra Stats 
```{r}
df_stats_nested <- df %>% filter(spu_filename %in% spu_spectra$spu_filename) %>% # only files with raw .spu data
  mutate(spectra_stats = map(Spectra, function(x) calculate_spectra_stats(x))) 

df_stats <- df_stats_nested %>% 
  unnest(spectra_stats)

## Plot high mean reflectance
df_stats %>% filter(Reflectance_mean > 0.9) %>% 
  filter(!str_detect(Treatment, "REF")) %>% 
  arrange(DateTime) %>% 
  slice(1:10) %>% plot_channels()

## Maxed
maxed_data <- df_stats %>% filter(ChA_max > 65000 | ChB_max > 65000) # list of files w/max'd spectra
maxed_data %>% select(Date, Site, spu_filename, Block, Treatment, Replicate, FileNum, DateTime, ChA_max, ChB_max, num_maxed) %>% summary()

```
### Year Specific Fixes 

#### 2014
```{r, fix_2014}
#------------------ Missing Replicate Info for 2014-08-13; MAT; B4; CT1, CT2, P, N

## Check Replicate NA's
df %>% filter(is.na(Replicate)) %>%
  filter(!str_detect(Treatment, "REF")) 

rep_vector <- rep(seq(1:5), 4)

## Created replacement subset
df_rep_fill <- df %>% filter(is.na(Replicate) & !str_detect(Treatment, "REF"))
df_rep_fill$Replicate <- factor(rep_vector)
df_rep_fill

## Remove and re-add subset
df <- df %>% filter(!(is.na(Replicate) & !str_detect(Treatment, "REF"))) %>% 
  bind_rows(df_rep_fill) 

#------------------ Replace Replicate = "V"
df <- df %>% 
  mutate(Replicate = replace(Replicate, Replicate == "V", "1")) %>% 
  mutate_at(c("Replicate"), funs(factor))


#------------------ UNRESOLVED: Check Replicate > 5
## Many have 6 Replicates
## 2014-06-30 HIST B3 NP -- 10 replicates
df %>% filter(as.integer(Replicate) > 6) %>% 
  select(Date, Site, spu_filename, Block, Treatment, Replicate, FileNum, DateTime)  %>% 
  print(n=100)

#------------------ Deal with Duplicates 
## WSG_2014-06-20_ALL.xlsx duplicates
df_dup_fix <- df %>% filter(xlsx_filename == "WSG_2014-06-20_ALL.xlsx") %>% 
  distinct(DateTime, spu_filename, .keep_all = T) 

df <- df %>% filter(xlsx_filename != "WSG_2014-06-20_ALL.xlsx") %>% 
  bind_rows(df_dup_fix)

## NANT_2014-06-21_ALL.xlsx jun21nant00044.spu       44 1         2014-06-21 NANT  2              P         NA      
## NANT_2014-06-21_ALL.xlsx jun21nant00044.spu       44 NA        2014-06-21 NANT  NA             REF2      NA 
### jun21nant00044.spu labeled as REF but actually is NOT. Is in fact 1st replicate of NANT, B2, P plot
df <- df %>% filter(!(str_detect(spu_filename, "jun21nant00044.spu") & Treatment == "REF2")) 

#------------------ UNRESOLVED Duplicates 
## LOF_2014-06-21_B1B2.xlsx jun21lofb100008.spu       8 NA        2014-06-21 LMAT  Specific Block REF1
### 7,8,9,10 are all REFERENCE scans : duplicate FileNum 8 should be changed to 9, 10
df %>% filter(str_detect(spu_filename, "jun21lofb1")) %>% 
  filter(FileNum < 12) %>% plot_channels()

## MAT_2014-06-26_ALL.xlsx  jun26matb400007.spu       7 NA        2014-06-26 MAT   NA             REF2   
### 6,7,8 are all REFERENCE scans : duplicate FileNum 7 should be changed to 8
spu_data %>% filter(str_detect(spu_filename, "jun26matb4")) %>% 
  filter(FileNum > 0 & FileNum < 9) %>% plot_channels()


```



#### 2015
```{r}
#------------------ Deal with Duplicates 

# 3 mislabeled references 
ref_mistake_files <- duplicates %>% pull(spu_filename) %>% unique() 

# WSG 2015-06-20: files 6, 7 mislabeled REFs -- should be 3 & 4
## Remove 5 & 6 as refs

wsg_refs <- spu_dataframe %>% filter(Site == "WSG", Date == "2015-06-20", FileNum %in% c(3,4)) %>% 
  mutate(Treatment = "REF1") %>% 
  select(-key, -spu_filename_full, -Remarks)

df[which(df$Site == "WSG" & df$Date == "2015-06-20" & df$FileNum %in% c(6,7) & str_detect(df$Treatment, "REF")), names(wsg_refs)] <- wsg_refs

df <- anti_join(df, duplicates %>% filter(str_detect(Treatment, "REF")))

rbind(df, wsg_refs)
#------------------ 10 Replicates for HIST dual labeled as reps 1-5

hst <- df %>% filter(Site == "HIST", !str_detect(Treatment, "REF")) %>% 
  arrange(DateTime)

## 10 files per plot
hst %>% group_by(Date, Site, Block, Treatment) %>% 
  summarize(Files = n_distinct(spu_filename), FileNums = str_c(FileNum, collapse = ",")) %>% print(n=100)

## Split into Reps 1-5 and 6-10
hst_1_5 <- hst %>% distinct(Date, Site, Block, Treatment, Replicate, .keep_all = T) %>% 
  mutate_at("Replicate", funs(as.numeric))

hst_6_10 <- anti_join(hst, hst_1_5, by = "spu_filename") %>% 
  mutate(Replicate = as.numeric(Replicate) + 5) # mutate 1-5 => 6-10

hst_rep_fix <- bind_rows(hst_1_5, hst_6_10) %>% arrange(DateTime)

## Modify dataframe
df <- df %>% filter(!(Site == "HIST" & !str_detect(Treatment, "REF"))) %>% # filter out rows
  mutate_at("Replicate", funs(as.numeric)) %>% 
  bind_rows(hst_rep_fix) %>%  # put them back in
  mutate_at("Replicate", funs(factor))


```

Rerun the "Check for Problems" chunk to make sure no errors remain. 




If you have already stepped through the above chunks and saved the resulting dataframes, you can skip to here to load the .rds & .csv files directly:
  
  - 2014 : "UnispecData/2014_unispec_dataframe.rds"
- 2015 : "UnispecData/2015_processed_data.rds"; 
- 2016 :
  - 2017 :
  - 2018 : 
  ```{r unispec_data}
processed_filename <- "UnispecData/2015_processed_data.rds"
unispec_dataframe <- read_rds(processed_filename)

```

### Save Joined Data
```{r}
# > Save joined data ------------------------------------------------------

## Save .rds dataframe
joined_filename <- paste0("UnispecData/", format(spu_data$DateTime[1], format="%Y"),"_processed_data.rds")
write_rds(unispec_dataframe, path = joined_filename)

## Save .csv of file key
unispec_keyname <- paste0("UnispecData/", format(unispec_dataframe$DateTime[1], format="%Y"),"_unispec_key_fix.csv")
write_csv(unispec_key, path = unispec_keyname)
```


## Save Dirty Spectral Data 
Problematic files labeled.
```{r unispec_key}
## Create file key to save? 
unispec_key <- df_problems %>%
  select(DateTime, Site, Block, Treatment, Replicate, FileNum, spu_filename, Weather, ref_problem, file_problem) %>%
  arrange(DateTime)

## Dirty files 
dirty_filename <- paste0("UnispecData/", format(df_dirty$DateTime[1], format="%Y"),"_unispec_dataframe_clean.rds")
write_rds(df_dirty, path = dirty_filename)

```

## Save Clean Spectral Data

```{r save_spectra, echo=T}
clean_unispec_dataframe <- df_clean %>%
  rename(spu_spectra = Spectra, multispec_indices = Indices) %>%
  select(Date, DateTime, Site, Block, Treatment, Replicate, FileNum, spu_filename, xlsx_filename, ref_filenames, Weather, Integration_ms, Temp, Type, multispec_spectra, multispec_indices, spu_spectra)


# SAVE CORRECTED SPECTRA if you want the dataframe
clean_filename <- paste0("UnispecData/", format(clean_unispec_dataframe$DateTime[1], format="%Y"),"_unispec_dataframe_clean.rds")
write_rds(clean_unispec_dataframe, path = clean_filename)

```

## Pattern of files to exclude
problem_files <- str_c(c(maxed_files, dim_files, absurd_files, zero_files, missing_spu_files), collapse = "|")

## Problems labeled
df_problems <- df %>%
  mutate(ref_problem = str_detect(ref_filenames, problem_files)) %>% # use str_extract_all to get filenames
  mutate(file_problem = str_detect(spu_filename, problem_files)) 

df_problems$problem_type = NA

df_problems$problem_type[df_problems$spu_filename %in% ref_mistake_files] <- "mislabeled_reference"
df_problems$problem_type[df_problems$spu_filename %in% maxed_files] <- "maxed"

df_problems %>% select(Date, Site, Block, Treatment, spu_filename, ref_problem, file_problem)
df_problems %>% select_if(function(x) typeof(x) != "list") %>% summary()

## Dirty Data
df_dirty <- df_problems %>%
  filter(ref_problem | file_problem)

## Clean Data
df_clean <- df_problems %>%
  filter(!ref_problem | is.na(ref_problem)) %>%
  filter(!file_problem) %>%  # remove bad files & those with ref problems
  select(-ref_problem, -file_problem)

df_clean %>% select_if(function(x) typeof(x) != "list") %>% summary()
unique(df_clean$Treatment)
df_dirty %>% select_if(function(x) typeof(x) != "list") %>% summary()
unique(df_dirty$Treatment)

## List Sites per Date table to display
df_clean_table <- df_clean %>%
  select(Date, Site) %>%
  distinct() %>%
  group_by(Date) %>%
  do(Sites = t(.[-1])) %>%
  mutate(Sites = paste( unlist(Sites), collapse=','))

kable(df_clean_table)
df_dirty

# REF problem list
ref_problems <- as.list(ref_problem_key$problems)
names(ref_problems) <- ref_problem_key$spu_filename

ref_key <- unispec_problem_key %>% 
  separate_rows(ref_filenames, sep = ", ") %>% 
  select(spu_filename, ref_filenames) %>% 
  mutate(ref_problem = recode(ref_filenames, !!!ref_problems, .default = NULL, .missing = NULL)) 

%>%
  group_by(spu_filename) %>% 
  summarize(filenames = str_c(unique(ref_filenames), collapse = "; "), problems = str_c(unique(ref_problem), collapse = "; ")) %>% 
  print(n=100)


ref_problems <- tibble(ref_filenames = ref_files) %>% 
  mutate(ref_problems = check_file_for_problems(ref_filenames, problem_files))

ref_filenames <- test$ref_filenames[100]
unlist(str_split(ref_filenames, pattern = ", "))



check_file_for_problems(ref_files[100], problem_files)

# ref_problem

unispec_problem_key %>% select_if(function(x) typeof(x) != "list") %>% summary()

ref_mistake_files <- ref_data_mistakes %>% select(spu_filename) %>% distinct() %>% pull()
maxed_files <- maxed_data_files_bad$spu_filename
low_files <- low_data_files$spu_filename
zero_files <- zero_data_files$spu_filename
duplicate_files <- duplicates$spu_filename

problem_files <- list(maxed = maxed_files,
                      dim = dim_files,
                      absurd = c(zero_files, absurd_files),
                      missing_spu = missing_spu_files, 
                      mislabeled = mislabeled_ref_files)

## filter maxed wavelengths from string
mutate(maxed_wavelengths = str_split(maxed_wavelengths, pattern = ", ")) %>% 
  mutate(waves = maxed_wavelengths %>% map(as.numeric)) %>% 
  unnest(waves)

%>%
  mutate(Date = lubridate::date(DateTime)) # add "Date" column in addition to DateTime

## Standardize Data
Convert dataframes to tidy format to clean. Standardize naming conventions / etc. across years. Check for read-in errors and mislabeled files.
```{r, df}
#### Check original un-standardized data with all columns
# df <- unispec_data

#### Standardize Dataframe
df <- unispec_dataframe %>%
  ## Remove columns
  # select(-Research_location, -Time, -Temp, -processed_filename) %>% #Research_location is always Toolik; Time is excel format; select(-c(ReflecMean, ReflecSERR, Number_ChAmax)) %>%  # unnecessary columns in older dataframes
  #select(-c(spu_filename_full, Remarks)) %>%
  ## Remove rows
  #filter(!is.na(spu_filename)) %>% # remove rows w/out spectral data
  # filter(!Type %in% c("Throwawayscan", "Darkscan", "not_in_process_file")) %>% # remove non-data
  # select(-Type) %>% 
  ## Mutate Rows 
  ### Fix filenumbers 
  mutate(FileNum = FileNum %% 100000) %>%  # correct for large filenumbers (>5 digits due to number at end of Site name)
  ### Convert Block from number to character 
  mutate(Block = ifelse(Block %in% c("1", "2", "3", "4"), str_c("B", Block), Block)) %>%
  ## Convert column type
  mutate_at(funs(factor), .vars = c("Site", "Block", "Treatment", "Replicate"))

```

## Save .csv key
unispec_key <- unispec_dataframe %>% 
  arrange(DateTime) %>% 
  select(spu_filename, Date, Site, Block, Treatment, Replicate, ref_filenames) %>%
  mutate(fix_Type = Type,
         fix_Site = Site, 
         fix_Block = Block,
         fix_Treatment = Treatment,
         fix_Replicate = Replicate,
         fix_ref_filenames = ref_filenames)
key_filename <-  paste0("UnispecData/", format(unispec_key$Date[1], format="%Y"),"_unispec_key_fix.csv")
write_csv(unispec_key, key_filename)


## MISSING 
# # raw spu file ? 
# mutate(missing_spu_file = is.na(spu_filename_full)) %>% 
# # multispec processed file ? 
# mutate(missing_multispec_file = is.na(xlsx_filename)) %>%


### Key column
# # > Create key column for processed data ----------------------------------
# 
# # Add a key variable of form yyy-mm-dd_site_key number (Date_Site_FileNum)
# key_info  <- key_info %>%  
#   mutate(key_num = ifelse (FileNum <999,  formatC(FileNum,width=5,flag="0"), FileNum),
#          key = paste0(format(Date, format="%Y-%m-%d"), "_", Site, "_", key_num)) %>% 
#   select(-key_num)

# # > Create key column for .spu data ---------------------------------------
# 
# # Create a key variable that is unique for each spu file
# spu_data <- spu_data %>% 
#   mutate(key_num = ifelse (FileNum <999,  formatC(FileNum,width=5,flag="0"), FileNum),
#          key = paste0(format(DateTime, format="%Y-%m-%d"), "_", Site, "_", key_num)) %>% 
#   select(-key_num)




### Missing and Unprocessed Files 
```{r}
### Find all .spu files not included in the Processed Files 
unprocessed_spu_data <- anti_join(spu_dataframe, key_info, by = "key") %>% 
  mutate( Treatment = NA) %>% #files not in key_info
  mutate( Treatment = ifelse(grepl("DARKscan",Remarks, fixed=T), "Darkscan", Treatment)) %>%
  mutate( Treatment = ifelse(grepl("Datascan,DC",Remarks, fixed=T), "Throwawayscan", Treatment)) %>%
  mutate(Type = "") %>% 
  #mutate(Type = if_else(ReflecMean>0.8,"Reference_scan","")) %>%   # Test for reference scan with a threshhold of 0.9
  mutate(Type = if_else(grepl("scan",Treatment),Treatment,
                        if_else(is.na(Treatment),"not_in_process_file",Type))) %>%    # add darkscan and throwaway scans
  mutate_at(c("Type", "Treatment", "Site"), funs(factor))

### List of unprocessed .spu data
unprocessed_spu_data %>% select_if(function(x) typeof(x) != "list") %>% summary()
unprocessed_spu_data %>% group_by(Date, Site) %>% 
  summarize(Types = str_c(unique(Type), collapse = ","), 
            #FileNums = str_c(unique(FileNum), collapse = ","), #list file numbers
            Files = n()) %>% 
  print(n=100)

### Find all processed data that does not have corresponding raw spu files 
missing_spu_data <- anti_join(key_info, spu_dataframe, by = "key") 

missing_spu_data %>% group_by(Date, Site) %>% 
  summarize(Treatments = str_c(unique(Treatment), collapse = ","), Files = n()) %>% 
  kable()


# > Save missing data  ----------------------------------------------------

write_rds(unprocessed_spu_data, path = paste0("UnispecData/", format(spu_data$DateTime[1], format="%Y"),"_unprocessed_spu_data.rds"))


write_rds(missing_spu_data, path = paste0("UnispecData/", format(spu_data$DateTime[1], format="%Y"),"_missing_spu_data.rds"))

```


### Join data present in both the multispec processed files and the raw .spu data
unispec_data <- inner_join(key_info, spu_dataframe) %>%  # does not include dark scans & throwaway scans
  mutate( Treatment = ifelse(grepl("DARKscan",Remarks, fixed=T), "Darkscan", Treatment)) %>%
  mutate( Treatment = ifelse(grepl("Datascan,DC",Remarks, fixed=T), "Throwawayscan", Treatment)) %>%
  mutate( Type = "") %>% # indicates Darkscans, Throwaway scans, and reference scans
  # mutate(Type = if_else(ReflecMean>0.8,"Reference_scan","")) %>%   # Test for reference scan with a threshhold of 0.9
  mutate(Type = if_else(grepl("scan",Treatment),Treatment,
                        if_else(is.na(Treatment),"not_in_process_file",Type))) %>%    # add darkscan and throwaway scans
  mutate_at(.vars = c("Type", "Treatment", "Site", "Replicate"), .funs = funs(factor))


### Plot Subset of References
```{r ref_plot}
## Plot a subset of reference spectra
first_file <- 1 # choose subset of files to plot
last_file <- 10 # avoid plotting >50 at a time

## Select a subset to plot
ref_subset <- ref_data_files %>%
  slice(first_file:last_file) %>%
  unnest(Spectra) %>%
  mutate(CorrectionFactor = 1/Reflectance) %>%
  gather(key = Channel, value = Intensity, ChB, ChA) %>%
  gather(key = ref_part, value = CorrectionFactor_Intensity, Intensity, CorrectionFactor)

ref_plot <- ggplot(data = ref_subset, mapping = aes(x = Wavelength, y = CorrectionFactor_Intensity)) +
  geom_line(aes(color=Treatment, linetype=Channel)) +
  facet_grid(ref_part ~ Date + Site + Treatment + Integration_ms + FileNum, scales="free")

ref_plot

```

## Plot maxed files 
## Plot max'd spectra
first_file <- 1 # choose subset of files to plot
last_file <-10 # > 20 is really slow / scrunched

maxed_spectra <- maxed_data_files %>%
  slice(first_file:last_file) %>% # don't plot too many
  unnest(Spectra) %>%
  gather(key = Channel, value = Intensity, ChB, ChA)

maxed_spectra_plot <- ggplot(maxed_spectra, aes(x=Wavelength, y= Intensity)) +
  geom_line(aes(color=Channel)) +
  geom_hline(yintercept = 65000, color="black",linetype=3) +
  facet_wrap(vars(Date,Site,Block,Treatment,FileNum), labeller = label_wrap_gen(multi_line=FALSE))

maxed_spectra_plot

# Compare multispec vs rscript --------------------------------------------

## Manual plot filtering selection to plot differences in Spectra
## MANUAL PLOT SELECTION
### change the following vectors to explore all data 
### facet based on selection to explore across blocks, sites, etc. 
### default plot has dates on the x-axis
sites <-c("MAT") 
blocks <- c("B4") # c("B1", "B2", "B3", "B4")
trtmts <- c("CT2") # c(CT, NP, "N", "P"), careful of CT1 & CT2 sites
reps <- c("1", "2", "3", "4")

## FILTER dataframe based on SELECTION
df_plot <- df_comp %>% 
  filter(Site %in% sites) %>% 
  filter(Block %in% blocks) %>% 
  filter(Treatment %in% trtmts) %>% 
  filter(Replicate %in% reps) %>% 
  filter(Wavelength > 400 & Wavelength < 1000)


## Comparing raw and corrected data different Wavelengths

df_comp_all <- df_rscript %>% # add raw spu & rscript_corrected  -- every 3.3nm
  unnest(Spectra) %>% 
  rename(Unispec_Reflectance = Reflectance) %>% # Wavelength every 3.3nm
  full_join(df_comp)%>% 
  # Nest reflectance information
  nest(Wavelength, ChA, ChB, Unispec_Reflectance, Multispec_Reflectance, Interp_Reflectance, Difference_Reflectance, Difference_Percent, .key = Spectra)

df_comp_all <- df_comp_all %>% unnest(Spectra)

## Review dates & sites
```{r}

df_list <- bind_rows(df_multispec %>% nest(Wavelength, Reflectance) %>% mutate(ProcessType = str_c("multispec_", Status)), df_rscript) %>% 
  separate(Status, sep = "_", into = c("ProcessType", "Status"), fill = "right") %>% 
  mutate(Status = replace(Status, is.na(Status), "raw")) %>% 
  mutate(ProcessType = replace(ProcessType, ProcessType == "r", "rscript")) %>% 
  mutate_at(vars(ProcessType, Status), funs(factor)) %>% 
  arrange(spu_filename)

df_table <- df_list %>%
  arrange(spu_filename) %>% 
  select(Date, Site, Status, ProcessType) %>%
  distinct() 

kable(df_table)
```

## Rename spectra columns
rename(spectra.raw = raw_spectra,
       indices.raw = raw_indices,
       spectra.r_corrected = corrected_spectra,
       indices.r_calculated.r_corrected = corrected_indices
)

## make comparison dataframes
load("UnispecData/multispec_data_2018.Rda")
load("UnispecData/rscript_data_2018.Rda")
df_multispec <- multispec_data_2018 %>% 
  nest(Wavelength, Reflectance, .key = Spectra) %>% 
  mutate(Type = ifelse(Type == "correct", "multispec_corrected", "multispec_raw"))



df_multispec <- multispec_data_2018 %>% 
  mutate(ProcessType = "multispec") %>% 
  #select(Date, Site, Block, Treatment, Measurement, Wavelength, Reflectance, Type) %>% 
  filter(Wavelength >= 400 & Wavelength <= 1100)  %>% # Choose relevent wavelengths 
  mutate(Site = replace(Site, Site %in% WSG, "WSG")) %>% # tidy : combine WSG1 & WSG23
  mutate(Site = replace(Site, Site %in% SHB, "SHB"))   # tidy : combine SHB1 & SHB2 

df_rscript <- rscript_data_2018 %>% 
  mutate(ProcessType = "rscript") %>% 
  filter(Wavelength >= 400 & Wavelength <= 1100)  %>% # Choose relevent wavelengths 
  mutate(Site = replace(Site, Site %in% WSG, "WSG")) %>% # tidy : combine WSG1 & WSG23
  mutate(Site = replace(Site, Site %in% SHB, "SHB"))  # tidy : combine SHB1 & SHB2 

  
# Check Clean Spectral & Veg Index Data -----------------------------------

# Check Spectral Data

## Plot Spectra: Site vs. Block
For the following code, make sure you select only one date per site. The 5 measurements per plot are averaged as a line and one standard deviation above and below shaded. Interactively edit the "PLOT SELECTION" vectors in the code chunk below to investigate specific data.


```{r plot_spectra_blocks, echo=F}
# Recall df_table for list of dates/sites
kable(df_table)

#PLOT SUBSET -- SPECIFY SITE/DATE/ETC to ZOOM IN ON
check_dates <- df_clean %>% distinct(Date) %>% pull()
check_sites <- c("MAT", "LMAT")
check_blocks <- c("B1", "B2", "B3", "B4")
check_trtmts <- c("NP", CT)

# Data Comparison Format
df_block <- df_clean %>%
  filter(Date %in% check_dates) %>%
  filter(Site %in% check_sites) %>%
  filter(Block %in% check_blocks) %>%
  filter(Treatment %in% check_trtmts) %>%
  unnest(Spectra) %>%
  group_by(Site, Block, Treatment, Date, Wavelength) %>%
  summarize(
    avg_reflect = mean(Reflectance),
    max_ref = max(Reflectance),
    min_ref = min(Reflectance),
    sd_reflect = sd(Reflectance),
    n = n()
  )

ggplot(data = df_block, mapping = aes(x = Wavelength, y = avg_reflect)) +
  geom_line(aes(color=Treatment)) +
  geom_ribbon(aes(ymin=avg_reflect-sd_reflect, ymax=avg_reflect+sd_reflect, fill=Treatment), alpha=0.25) +
  facet_grid(Date ~ Site + Block ) +
  scale_color_manual(values=c("CT" = "forestgreen", "CT1"="darkolivegreen2", "CT2"="darkolivegreen3",
                              "N" = "dodgerblue", "NO3" = "skyblue", "NH4" = "deepskyblue",
                              "P" = "red2",
                              "NP" = pur_pal[5],
                              "F0.5" = pur_pal[1],
                              "F1" = pur_pal[2],
                              "F2" = pur_pal[3],
                              "F5" = pur_pal[4],
                              "F10" = pur_pal[5])) +
  scale_fill_manual(values=c("CT" = "forestgreen", "CT1"="darkolivegreen2", "CT2"="darkolivegreen3",
                             "N" = "dodgerblue", "NO3" = "skyblue", "NH4" = "deepskyblue",
                             "P" = "red2",
                             "NP" = pur_pal[5],
                             "F0.5" = pur_pal[1],
                             "F1" = pur_pal[2],
                             "F2" = pur_pal[3],
                             "F5" = pur_pal[4],
                             "F10" = pur_pal[5]))

```



## Plot Spectra: Site vs. Date
Average plot averages by block. NOTE: Ask Laura what the correct error propogation is here.

```{r plot_spectra_dates, echo=F}
#PLOT SUBSET -- SPECIFY SITE/DATE/ETC to ZOOM IN ON
check_dates <- df_clean %>% distinct(Date) %>% pull() #ymd("2016-06-18", "2016-06-23")
check_sites <- c("MAT", "LMAT")
check_blocks <- c("B1", "B2", "B3", "B4")
check_trtmts <- c("NP", CT)

# Data Comparison Format
df_dates <- df_clean %>%
  filter(Date %in% check_dates) %>%
  filter(Site %in% check_sites) %>%
  filter(Block %in% check_blocks) %>%
  filter(Treatment %in% check_trtmts) %>%
  unnest(Spectra) %>%
  group_by(Site, Block, Treatment, Date, Wavelength) %>% # average measurements by plot
  summarize(
    avg_reflect = mean(Reflectance),
    max_ref = max(Reflectance),
    min_ref = min(Reflectance),
    sd_reflect = sd(Reflectance)
  )  %>%
  group_by(Site, Treatment, Date, Wavelength) %>% # average plots by block
  summarize(
    block_avg_reflect = mean(avg_reflect),
    max_ref = max(avg_reflect),
    min_ref = min(avg_reflect),
    sd_reflect = sd(avg_reflect)
  )

ggplot(data = df_dates, mapping = aes(x = Wavelength, y = block_avg_reflect)) +
  geom_line(aes(color=Treatment)) +
  geom_ribbon(aes(ymin=block_avg_reflect-sd_reflect, ymax=block_avg_reflect+sd_reflect, fill=Treatment), alpha=0.25) +
  facet_grid(Site ~ Date) +
  scale_color_manual(values=c("CT" = "forestgreen", "CT1"="darkolivegreen2", "CT2"="darkolivegreen3",
                              "N" = "dodgerblue", "NO3" = "skyblue", "NH4" = "deepskyblue",
                              "P" = "red2",
                              "NP" = pur_pal[5],
                              "F0.5" = pur_pal[1],
                              "F1" = pur_pal[2],
                              "F2" = pur_pal[3],
                              "F5" = pur_pal[4],
                              "F10" = pur_pal[5])) +
  scale_fill_manual(values=c("CT" = "forestgreen", "CT1"="darkolivegreen2", "CT2"="darkolivegreen3",
                             "N" = "dodgerblue", "NO3" = "skyblue", "NH4" = "deepskyblue",
                             "P" = "red2",
                             "NP" = pur_pal[5],
                             "F0.5" = pur_pal[1],
                             "F1" = pur_pal[2],
                             "F2" = pur_pal[3],
                             "F5" = pur_pal[4],
                             "F10" = pur_pal[5]))

```


# Check Vegetation Indices

Currently, this only works for NDVI, EVI, and EVI2 as I haven't worked out spectral interpolation yet and the other indices need reflectance at a specific value (not a range).

## Extract NDVI data
```{r convert_ndvi}
## Only NDVI data
ndvi_data <- df_clean %>%  # select just NDVI
unnest(Indices) %>%
filter(Index == "NDVI(MODIS)") %>%
rename(NDVI = Value)
```

## Plot NDVI: Site vs. Block
```{r ndvi, echo=F}
#PLOT SUBSET -- SPECIFY SITE/DATE/ETC to ZOOM IN ON
check_dates <- ndvi_data %>% select(Date) %>% unique() %>%  pull()#ymd("2017-06-08", "2017-06-15", "2017-06-26")
check_sites <- c("MAT")
check_blocks <- c("B1", "B2", "B3", "B4")
check_trtmts <- c("NP", "F10", CT)

# ndvi_types <- df_tidy %>% # to calculate NDVI
#   filter(Date %in% check_dates) %>%
#   filter(Site %in% check_sites) %>%
#   filter(Block %in% check_blocks) %>%
#   filter(Treatment %in% check_trtmts) %>%
#   calculate_index(indices = "NDVI") # function in "unispec_functions.R"

ndvi_plot <- ndvi_data %>%
filter(Date %in% check_dates) %>%
filter(Site %in% check_sites) %>%
filter(Block %in% check_blocks) %>%
filter(Treatment %in% check_trtmts) %>%
group_by(Site, Block, Treatment, Date) %>%
summarise(
avg_ndvi = mean(NDVI),
sd_ndvi = sd(NDVI)
) %>%
group_by(Site,Treatment,Date) %>%
summarise(
avg_ndvi = mean(avg_ndvi),
sd_ndvi = sd(sd_ndvi)
)

ndvi_subset <- ndvi_data %>%
filter(Date %in% check_dates)%>%
filter(Site %in% check_sites) %>%
filter(Block %in% check_blocks)%>%
filter(Treatment %in% check_trtmts) %>%
group_by(Block)

ggplot(data = ndvi_subset, mapping = aes(x = Date, y = NDVI, color = Treatment)) +
geom_point(aes(shape=Block), position = "jitter") +
geom_smooth(aes(fill=Treatment), method = lm, formula = y ~ splines::bs(x, 3)) +
facet_wrap(vars(Site)) +
scale_color_manual(values=c("CT" = "forestgreen", "CT1"="darkolivegreen2", "CT2"="darkolivegreen3",
"N" = "dodgerblue", "NO3" = "skyblue", "NH4" = "deepskyblue",
"P" = "red2",
"NP" = pur_pal[5],
"F0.5" = pur_pal[1],
"F1" = pur_pal[2],
"F2" = pur_pal[3],
"F5" = pur_pal[4],
"F10" = pur_pal[5])) +
scale_fill_manual(values=c("CT" = "forestgreen", "CT1"="darkolivegreen2", "CT2"="darkolivegreen3",
"N" = "dodgerblue", "NO3" = "skyblue", "NH4" = "deepskyblue",
"P" = "red2",
"NP" = pur_pal[5],
"F0.5" = pur_pal[1],
"F1" = pur_pal[2],
"F2" = pur_pal[3],
"F5" = pur_pal[4],
"F10" = pur_pal[5]))


plot_errorbar <- ggplot(data = ndvi_plot, mapping = aes(x = Date, y = avg_ndvi, color=Treatment, fill=Treatment)) +
geom_point() +
geom_line() +
geom_errorbar(aes(ymin = avg_ndvi-sd_ndvi, ymax= avg_ndvi + sd_ndvi), width=2) +
facet_wrap(vars(Site)) +
scale_color_manual(values=c("CT" = "forestgreen", "CT1"="darkolivegreen2", "CT2"="darkolivegreen3",
"N" = "dodgerblue", "NO3" = "skyblue", "NH4" = "deepskyblue",
"P" = "red2",
"NP" = pur_pal[5],
"F0.5" = pur_pal[1],
"F1" = pur_pal[2],
"F2" = pur_pal[3],
"F5" = pur_pal[4],
"F10" = pur_pal[5]))
```




## Calculate Indices from Spectra
```{r df_ndvi}
# build tibble of #band_defns: instrument, color, min, max
band_defns <- tribble(
~definition, ~color, ~min, ~max,
"ITEX", "red", 560, 600,
"ITEX", "nir", 725, 1000,
"MODIS", "red", 620, 670,
"MODIS", "nir", 841, 876,
"MODIS", "blue", 459,479,
"SKYE", "red", 620, 680,
"SKYE", "nir", 830, 880,
"SKYE", "blue", 455, 480,
"ToolikGIS_Drone_2018", "red", 640, 680,
"ToolikGIS_Drone_2018", "nir", 820, 890
)

ndvi_from_spectra <- clean_unispec_dataframe %>%
filter(!str_detect(Treatment, "REF")) %>%
mutate(spu_indices = map(spu_spectra, function(x) calculate_indices(x, band_defns = band_defns, instrument = "MODIS", indices = "NDVI")))

df_ndvi  <- ndvi_from_spectra %>%
# mutate(multispec_indices = map(multispec_indices, function(x) filter(x, Index=="NDVI(MODIS)"))) %>%
unnest(spu_indices) %>%
mutate(Index = str_c(Index, "_raw")) %>%
unnest(multispec_indices) %>%
mutate(Index1 = str_c(Index1, "_corrected")) %>%
gather(key = temp, value = Index, Index, Index1) %>% # combine indices into one column
gather(key = temp, value = Value, Value, Value1) %>%
select(-temp)

```

## Compare Raw vs Corrected NDVI
```{r ndvi_comp}

#PLOT SUBSET -- SPECIFY SITE/DATE/ETC to ZOOM IN ON
check_dates <- df_ndvi %>% select(Date) %>% unique() %>%  pull()#ymd("2017-06-08", "2017-06-15", "2017-06-26")
check_sites <- c("MAT", "LMAT", "HIST")
check_blocks <- c("B1", "B2", "B3", "B4")
check_trtmts <- c(CT, "NP")


df_ndvi_subset <- df_ndvi %>%
filter(Date %in% check_dates)%>%
filter(Site %in% check_sites) %>%
filter(Block %in% check_blocks)%>%
filter(Treatment %in% check_trtmts) %>%
filter(str_detect(Index, "NDVI"))

df_ndvi_summary <- df_ndvi_subset %>%
## optional: group CT plots
mutate(Treatment = replace(Treatment, Treatment %in% CT, "CT")) %>%  
## average data
group_by(Site, Block, Treatment, Date, Index) %>%
summarise(
avg_ndvi = mean(Value),
sd_ndvi = sd(Value)
) %>%
group_by(Site,Treatment,Date,Index) %>%
summarise(
avg_ndvi = mean(avg_ndvi),
sd_ndvi = sd(sd_ndvi)
)

ggplot(data = df_ndvi_summary, mapping = aes(x = Date, y = avg_ndvi, color=Index)) +
geom_point() +
geom_line() +
geom_errorbar(aes(ymin = avg_ndvi-sd_ndvi, ymax= avg_ndvi + sd_ndvi)) +
facet_grid(Treatment ~ Site)

```


## Save Vegetation Index Data
Append data to yearly r dataframe or .csv file. Write code to read in saved data and remove duplicate rows. >> ASK JIM: I'm not sure what the best data organization system is yet.

# process_unispec_record scrap --------------------------------------------


## Metadata Check
```{r}
df %>% 
  filter(Site == "NANT") %>% 
  filter(Date == lubridate::ymd("2014-06-21")) %>% 
  #filter(FileNum > 0 & FileNum < 100) %>% 
  select(Date, Site, spu_filename, Block, Treatment, Replicate, FileNum, DateTime)  %>% 
  print(n = 100) 
# select_if(function(x) typeof(x) != "list") %>% summary()


```



## Plot Check
Run the following code chunk interactively in RStudio to check references at specific sites/dates.
```{r check_refs}
## Load raw spu data (to check those that weren't processed) from "read_process_raw_files_vRuby.R"
raw_data <- read_rds("HistoricUnispecData/2016_raw_spu_dataframe.rds")

## SPECIFY SITE/DATE/ETC to ZOOM IN ON
check_site <- "LMAT"
check_date <- as_date("2016-05-26") # need to be in data format
first_file <- 0
last_file <- 100

ref_check <- raw_data %>% # full dataframe without maxed filtered
  filter(Date %in% check_date) %>%
  filter(Site %in% check_site) %>%  
  filter(FileNum >= first_file) %>%
  filter(FileNum <= last_file) %>%
  left_join(df %>% select(-Spectra, -Indices)) %>%
  filter(str_detect(Treatment, "REF") | is.na(Treatment))

plot_check <- ref_check %>%
  unnest(Spectra) %>%
  gather(key = Channel, value = Intensity, ChB, ChA) %>%
  gather(key = ref_part, value = Reflectance_Intensity, Intensity, Reflectance)

## Plot Specified Correction Factors for quality check
plot_zoom <- ggplot(data = plot_check, mapping = aes(x = Wavelength, y = Reflectance_Intensity)) +
  geom_line(aes(color=Treatment, linetype=Channel)) +
  facet_grid(ref_part ~ Date + Site + FileNum + Integration_ms, scales="free")

plot_zoom
```

#----------------------------------------------------------------------------------------------
## Standardize Names Chunk
```{r standardize_names, dependson="convert"}
## Useful vectors for standardizing site names
WSG <- c("WSG1", "WSG23")
SHB <- c("SHB1", "SHB2")
HST <- c("HIST")
HTH <- c("DHT")

## Standard Treatment names 
CT <- c("CT","CT1","CT2")
NP_gradient <- c("F0.5","F1","F2","F5","F10") # for LOF site
N_types <- c("NO3", "NH4") # for LOF site
trtmt_list <- list(CT, "N", "P", "NP", NP_gradient, N_types)

## Check treatment names
trtmt_list_df <- df %>% select(Treatment) %>% unique() %>% pull() # all treatments
trtmt_list_df

## Standardize names
df_tidy <- df %>%
  mutate(Site = replace(Site, Site %in% WSG, "WSG")) %>% # rename WSG1 & WSG23 to WSG
  mutate(Site = replace(Site, Site %in% SHB, "SHB")) %>%  # rename  SHB1 & SHB2 to SHB
  mutate(Site = replace(Site, Site %in% HTH, "HTH")) %>%
  mutate(Site = replace(Site, Site %in% HIST, "HIST"))  %>%
  # Block Names
  mutate(Block = ifelse(Block %in% c("1", "2", "3", "4"), str_c("B", Block), Block)) %>%
  mutate_at(vars(Block), .funs = funs(factor))
# Treatment Names
# mutate(Treatment = ifelse(Treatment %in% CT),"CT", Treatment))


```


df_tidy <- df %>% 
  gather(key = Index, value = Value, 'NDVI(MODIS)':'ChlIndex') %>%
  select(-Spectra) %>% # can't nest w/list column -- remove for now
  nest(Index, Value, .key = "Indices") %>%
  inner_join(df_data %>% select(-c('NDVI(MODIS)':'ChlIndex')))  # rejoin w/Spectra column

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


