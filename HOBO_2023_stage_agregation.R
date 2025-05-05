# Read Stage data for 2023 

# Creator: Lara Munro
# Date: December 12 2024


# Load libraries
library(tidyverse)

# Parts: 
# 1. Read barometric data
# 2. Read silver body HOBO 
# 3. Read black body HOBO
# 4. Read EXO
# 5. Convert pressure to depth
# 6. Calculate depth offsets
# 7. Export data

dataloc <- "C:/Users/laram/OneDrive - USNH/CLGB/"


# 1. Read barometric data -------------------------------------------------

# Read barometrice pressure data
# Get this data from Jody Potter at WQAL, make sure that units are the same 
# This data includes 2022 and 2023 in the same data file. It has been included in the 2022 and 2023 baro folder.
baroloc <- paste(dataloc, "WHB Barometric/annual/WHB_atm_Pressure2023.csv", sep = "")
dat <- read.csv(baroloc)
dat$DateTime <- as.POSIXct(dat$datetime, tz = "EST",
                           format = "%Y-%m-%d %H:%M")
dat <- dat %>%
  rename(pres.kpa.baro = pres.kpa,
         temp.C.baro = temp.C)
dat <- na.omit(dat)
dat <- dat[c("DateTime", "pres.kpa.baro", "temp.C.baro")]

# 2. Read HOBO data ------------------------------------------------

# Read tables from 2023 
staloc <- paste0(dataloc, "CLGB.AG/data/stage/y2023/rawData/")
flist <- list.files(staloc, pattern = "*.csv")
flist

# Read the first stage file
dfloc <- paste0(staloc, flist[1])
sta <- read.csv(dfloc, skip = 1)
sta <- sta[2:4]
sta <- sta %>%
  rename(DateTime = Date.Time..GMT.05.00,
         pres.kpa = Abs.Pres..kPa..LGR.S.N..1303191..SEN.S.N..1303191.,
         temp.C = Temp...C..LGR.S.N..1303191..SEN.S.N..1303191.)
sta$DateTime <- as.POSIXct(sta$DateTime, tz = "EST",
                           format = "%m/%d/%y %I:%M:%S %p")
sta <- na.omit(sta)

# Read the SECOND stage file
dfloc <- paste0(staloc, flist[2])
sta2 <- read.csv(dfloc, skip = 1)
sta2 <- sta2[2:4]
sta2 <- sta2 %>%
  rename(DateTime = Date.Time..GMT.05.00,
         pres.kpa = Abs.Pres..kPa..LGR.S.N..1303191..SEN.S.N..1303191.,
         temp.C = Temp...C..LGR.S.N..1303191..SEN.S.N..1303191.)
sta2$DateTime <- as.POSIXct(sta2$DateTime, tz = "EST",
                            format = "%m/%d/%y %I:%M:%S %p")
sta2 <- na.omit(sta2)

# Combine old data into single data frame
sta <- rbind(sta, sta2)

# Read the THIRD stage file
dfloc <- paste0(staloc, flist[3])
sta2 <- read.csv(dfloc, skip = 1)
sta2 <- sta2[2:4]
sta2 <- sta2 %>%
  rename(DateTime = Date.Time..GMT.05.00,
         pres.kpa = Abs.Pres..kPa..LGR.S.N..1303191..SEN.S.N..1303191.,
         temp.C = Temp...C..LGR.S.N..1303191..SEN.S.N..1303191.)
sta2$DateTime <- as.POSIXct(sta2$DateTime, tz = "EST",
                            format = "%m/%d/%y %I:%M:%S %p")
sta2 <- na.omit(sta2)

# Combine black body data into single data frame
sta <- rbind(sta, sta2)

# Read the FOURTH stage file
dfloc <- paste0(staloc, flist[4])
sta2 <- read.csv(dfloc, skip = 1)
sta2 <- sta2[2:4]
sta2 <- sta2 %>%
  rename(DateTime = Date.Time..GMT.05.00,
         pres.kpa = Abs.Pres..kPa..LGR.S.N..1303191..SEN.S.N..1303191.,
         temp.C = Temp...C..LGR.S.N..1303191..SEN.S.N..1303191.)
sta2$DateTime <- as.POSIXct(sta2$DateTime, tz = "EST",
                            format = "%m/%d/%y %I:%M:%S %p")
sta2 <- na.omit(sta2)

# Combine old data into single data frame
sta <- rbind(sta, sta2)

# Merge baro and silver HOBO data
dat <- merge(dat, sta, by = "DateTime", all.x = TRUE)



# 4. Convert pressure to depth --------------------------------------------

# Conversion factors
FEET_TO_METERS = 0.3048
KPA_TO_PSI = 0.1450377
PSI_TO_PSF = 144.0


# water pressure
dat$watPres.kPa <- dat$pres.kpa - dat$pres.kpa.baro

# Fluid density calculation (from Reese LeVea)
dat$density = (999.83952 + (16.945176 * dat$temp.C) - 
                     (7.9870401e-03 * dat$temp.C**2) - 
                     (46.170461e-06 * dat$temp.C**3) + 
                     (105.56302e-09 * dat$temp.C**4) - 
                     (280.54253e-12 * dat$temp.C**5)) /
  (1 + 16.879850e-03 * dat$temp.C) # density in kg/m3

# Convert density from kg/m3 to lb/ft^3
dat$density.lbft <- 0.0624279606 * dat$density

# Convert pressure to density-dependent fluid depth array
dat$depth.m = (FEET_TO_METERS) * (KPA_TO_PSI * PSI_TO_PSF * dat$watPres.kPa) / dat$density.lbft

dat[sapply(dat, is.infinite)] <- NA

# 5. Export data ----------------------------------------------------------
dat2 <- dat

dat$DateTime <- as.character(format(dat$DateTime))
write.csv(dat, paste0(dataloc,"/CLGB.AG/data/ratingCurve/CLGB.AG_2023_waterDepths.csv"), row.names = F)

