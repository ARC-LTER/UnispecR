

# Required Packages -------------------------------------------------------
library(lubridate)
library(tidyverse)


# 2017-2018 Data Organization -----------------------------------------------

## Renaming SPU directories to Universal Datetime format YEAR-MONTH-DAY (digits)

setwd("UnispecData/2017/spu/")

dirs <- list.files(path=getwd(), recursive = F)
new_dirs <- as_date(dirs, format = "%d%B%Y", tz = "US/Alaska")

file.rename(dirs, as.character(new_dirs))
