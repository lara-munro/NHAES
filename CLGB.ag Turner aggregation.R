# Turner data aggregation
# Author: Lara Munro
# Date: August 2025

# Script reads Turner data that was previously aggregated by C. Whitney and creates the final data file


# Load relevant libraries
library(tidyverse)
library(ggplot2)

# Set data locations

dataloc <- "C:/Users/laram/OneDrive - USNH/CLGB/"


# 2022 --------------------------------------------------------------------
tuloc <- paste(dataloc, "CLGB.AG/data/fdom/y2022/rawData/CLGB_AG_Turner_2022-06-10-to-2022-11-28.csv", sep = "")
tur <- read.csv(tuloc)
tur$DateTime <- as.POSIXct(tur$DateTime, tz = "EST",
                           format = "%m/%d/%Y %H:%M")
tur <- tur %>%
  rename(DateTime.EST = DateTime)
         
# Convert date time to a format that will save properly
tur$DateTime.EST <-  as.character(format(tur$DateTime.EST))

write.csv(tur, paste0(dataloc, "CLGB.AG/data/fdom/y2022/finalData/CLGBag_fdom_2022.csv"), row.names = FALSE)

# 2023 --------------------------------------------------------------------
tuloc <- paste(dataloc, "CLGB.AG/data/fdom/y2023/intermediateData/CLGB_AG_2023-07-18-to-2023-08-04.csv", sep = "")
tur <- read.csv(tuloc)
tur$DateTime <- as.POSIXct(tur$DateTime, tz = "EST",
                           format = "%m/%d/%y %H:%M")
tur <- tur %>%
  rename(DateTime.EST = DateTime)

# Convert date time to a format that will save properly
tur$DateTime.EST <-  as.character(format(tur$DateTime.EST))

write.csv(tur, paste0(dataloc, "CLGB.AG/data/fdom/y2023/finalData/CLGBag_fdom_2023.csv"), row.names = FALSE)

