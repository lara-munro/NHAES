# CLGB.Upper conductivity


# Author: Lara Munro
# Date: August 2025

# Script aggregates conductivity measurements from the HOBO data logger at CLGB.Ag into annual data tables
# Data files need to be converted into .CSVs in HOBOWare
# Loops are set up for data tables exported into EST (GMT -5.00) and for the logger withe the serial number: 20625421
# If the loops do not work, check time zones and logger serial number

# Set up R space --------------------------------------------------

# Load relevant libraries
library(tidyverse)
library(ggplot2)

# Set data locations
dataloc <- "C:/Users/laram/OneDrive - USNH/CLGB/"


# 2024 --------------------------------------------------------------------
condloc <- paste0(dataloc, "CLGB.UP/conductivity/y2024/intermediateData/")
flist <- list.files(condloc, pattern = "*_cond.csv")
flist

dfloc <- paste0(condloc, flist[1])
cond <- read.csv(dfloc, skip = 1)
cond <- cond[2:4]
cond <- cond %>%
  rename(DateTime.EST = Date.Time..GMT.05.00,
         cond.uScm = Full.Range..μS.cm..LGR.S.N..10065590..SEN.S.N..10065590..LBL..CondFullRange.,
         temp.water.C = Temp...C..LGR.S.N..10065590..SEN.S.N..10065590..LBL..Temp_C.)
cond$DateTime.EST <- as.POSIXct(cond$DateTime.EST, tz = "EST",
                                format = "%m/%d/%y %I:%M:%S %p")
cond <- na.omit(cond)
cond$condSerialNb <- "10065590"


# Loop to aggregate every other conductivity file
# If the loop doesn't work check: time shift from GMT - should be -5
# Serial number of the logger - raw column names include the serial number
for(i in 2:length(flist)){
  dfloc <- paste0(condloc, flist[i])
  cond2 <- read.csv(dfloc, skip = 1)
  cond2 <- cond2[2:4]
  cond2 <- cond2 %>%
    rename(DateTime.EST = Date.Time..GMT.05.00,
           cond.uScm = Full.Range..μS.cm..LGR.S.N..10065590..SEN.S.N..10065590..LBL..CondFullRange.,
           temp.water.C = Temp...C..LGR.S.N..10065590..SEN.S.N..10065590..LBL..Temp_C.)
  cond2$DateTime.EST <- as.POSIXct(cond2$DateTime.EST, tz = "EST",
                                  format = "%m/%d/%y %I:%M:%S %p")
  cond2 <- na.omit(cond2)
  cond2$condSerialNb <- "10065590"
  
  # Merge files together
  cond <- rbind(cond, cond2)
  cond <- cond[!duplicated(cond), ]
  
  i = i +1
}

# Remove data from 2025
cond <- cond[cond$DateTime.EST < as.POSIXct("2025-01-01"), ]

# Convert date time to a format that will save properly
cond$DateTime.EST <-  as.character(format(cond$DateTime.EST))

write.csv(cond, paste0(dataloc, "CLGB.UP/conductivity/y2024/finalData/CLGBup_COND_2024.csv"), row.names = FALSE)


# 2025 --------------------------------------------------------------------
condloc <- paste0(dataloc, "CLGB.UP/conductivity/y2025/intermediateData/")
flist <- list.files(condloc, pattern = "*_cond.csv")
flist

dfloc <- paste0(condloc, flist[1])
cond <- read.csv(dfloc, skip = 1)
cond <- cond[2:4]
cond <- cond %>%
  rename(DateTime.EST = Date.Time..GMT.05.00,
         cond.uScm = Full.Range..μS.cm..LGR.S.N..10065590..SEN.S.N..10065590..LBL..CondFullRange.,
         temp.water.C = Temp...C..LGR.S.N..10065590..SEN.S.N..10065590..LBL..Temp_C.)
cond$DateTime.EST <- as.POSIXct(cond$DateTime.EST, tz = "EST",
                                format = "%m/%d/%y %I:%M:%S %p")
cond <- na.omit(cond)
cond$condSerialNb <- "10065590"


# Loop to aggregate every other conductivity file
# If the loop doesn't work check: time shift from GMT - should be -5
# Serial number of the logger - raw column names include the serial number
for(i in 2:length(flist)){
  dfloc <- paste0(condloc, flist[i])
  cond2 <- read.csv(dfloc, skip = 1)
  cond2 <- cond2[2:4]
  cond2 <- cond2 %>%
    rename(DateTime.EST = Date.Time..GMT.05.00,
           cond.uScm = Full.Range..μS.cm..LGR.S.N..10065590..SEN.S.N..10065590..LBL..CondFullRange.,
           temp.water.C = Temp...C..LGR.S.N..10065590..SEN.S.N..10065590..LBL..Temp_C.)
  cond2$DateTime.EST <- as.POSIXct(cond2$DateTime.EST, tz = "EST",
                                   format = "%m/%d/%y %I:%M:%S %p")
  cond2 <- na.omit(cond2)
  cond2$condSerialNb <- "10065590"
  
  # Merge files together
  cond <- rbind(cond, cond2)
  cond <- cond[!duplicated(cond), ]
  
  i = i +1
}

# Remove data from 2025
cond <- cond[cond$DateTime.EST > as.POSIXct("2025-01-01"), ]

# Convert date time to a format that will save properly
cond$DateTime.EST <-  as.character(format(cond$DateTime.EST))

write.csv(cond, paste0(dataloc, "CLGB.UP/conductivity/y2025/finalData/CLGBup_COND_2025.csv"), row.names = FALSE)
