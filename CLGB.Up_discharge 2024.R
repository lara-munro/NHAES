# CLGB.Upper discharge 2024
# Author: Lara Munro
# Date: August 2025

# Load relevant libraries
library(tidyverse)
library(ggplot2)

# Set data location
dataloc <- "C:/Users/laram/OneDrive - USNH/CLGB/"


# 1. Read barometric data -------------------------------------------------

# Read barometrice pressure data
# Get this data from Jody Potter at WQAL, make sure that units are the same 
baroloc <- paste(dataloc, "Barometer/annual/WHB_atm_Pressure2024.csv", sep = "")
barodat <- read.csv(baroloc)
barodat$DateTime <- as.POSIXct(barodat$DateTime, tz = "EST",
                               format = "%Y-%m-%d %H:%M")
barodat <- barodat %>%
  rename(DateTime.EST = DateTime,
         pres.baro.kpa = pres.kpa,
         temp.baro.C = temp.C)
barodat <- na.omit(barodat)


# 2. Read HOBO data -------------------------------------------------------
staloc <- paste0(dataloc, "CLGB.UP/stage/y2024/intermediateData/")
flist <- list.files(staloc, pattern = "*_stage.csv")
flist

# Read the first HOBO file
dfloc <- paste0(staloc, flist[1])
sta <- read.csv(dfloc, skip = 1)
sta <- sta[2:4]
sta <- sta %>%
  rename(DateTime.EST = Date.Time..GMT.05.00,
         pres.water.kpa  = Abs.Pres..kPa..LGR.S.N..20434532..SEN.S.N..20434532.,
         temp.water.C = Temp...C..LGR.S.N..20434532..SEN.S.N..20434532.)
sta$DateTime.EST <- as.POSIXct(sta$DateTime.EST, tz = "EST",
                               format = "%m/%d/%y %I:%M:%S %p")
sta <- na.omit(sta)
sta$stageDevice <- "black HOBO"
sta$stageSerialNb <- "20434532"

# Read the second HOBO file
dfloc <- paste0(staloc, flist[2])
sta2 <- read.csv(dfloc, skip = 1)
sta2 <- sta2[2:4]
sta2 <- sta2 %>%
  rename(DateTime.EST = Date.Time..GMT.05.00,
         pres.water.kpa  = Abs.Pres..kPa..LGR.S.N..20434532..SEN.S.N..20434532.,
         temp.water.C = Temp...C..LGR.S.N..20434532..SEN.S.N..20434532.)
sta2$DateTime.EST <- as.POSIXct(sta2$DateTime.EST, tz = "EST",
                                format = "%m/%d/%y %I:%M:%S %p")
sta2 <- na.omit(sta2)
sta2$stageDevice <- "black HOBO"
sta2$stageSerialNb <- "20434532"

# Combine HOBO data into single data frame
sta <- rbind(sta, sta2)


# Read the third HOBO file
dfloc <- paste0(staloc, flist[3])
sta2 <- read.csv(dfloc, skip = 1)
sta2 <- sta2[2:4]
sta2 <- sta2 %>%
  rename(DateTime.EST = Date.Time..GMT.05.00,
         pres.water.kpa  = Abs.Pres..kPa..LGR.S.N..20434532..SEN.S.N..20434532.,
         temp.water.C = Temp...C..LGR.S.N..20434532..SEN.S.N..20434532.)
sta2$DateTime.EST <- as.POSIXct(sta2$DateTime.EST, tz = "EST",
                                format = "%m/%d/%y %I:%M:%S %p")
sta2 <- na.omit(sta2)
sta2$stageDevice <- "black HOBO"
sta2$stageSerialNb <- "20434532"

# Combine HOBO data into single data frame
sta <- rbind(sta, sta2)

# Read the fourth HOBO file
dfloc <- paste0(staloc, flist[4])
sta2 <- read.csv(dfloc, skip = 1)
sta2 <- sta2[2:4]
sta2 <- sta2 %>%
  rename(DateTime.EST = Date.Time..GMT.05.00,
         pres.water.kpa  = Abs.Pres..kPa..LGR.S.N..20434532..SEN.S.N..20434532.,
         temp.water.C = Temp...C..LGR.S.N..20434532..SEN.S.N..20434532.)
sta2$DateTime.EST <- as.POSIXct(sta2$DateTime.EST, tz = "EST",
                                format = "%m/%d/%y %I:%M:%S %p")
sta2 <- na.omit(sta2)
sta2$stageDevice <- "black HOBO"
sta2$stageSerialNb <- "20434532"

# Combine HOBO data into single data frame
sta <- rbind(sta, sta2)

# Read the fifth HOBO file
dfloc <- paste0(staloc, flist[5])
sta2 <- read.csv(dfloc, skip = 1)
sta2 <- sta2[2:4]
sta2 <- sta2 %>%
  rename(DateTime.EST = Date.Time..GMT.05.00,
         pres.water.kpa  = Abs.Pres..kPa..LGR.S.N..20434532..SEN.S.N..20434532.,
         temp.water.C = Temp...C..LGR.S.N..20434532..SEN.S.N..20434532.)
sta2$DateTime.EST <- as.POSIXct(sta2$DateTime.EST, tz = "EST",
                                format = "%m/%d/%y %I:%M:%S %p")
sta2 <- na.omit(sta2)
sta2$stageDevice <- "black HOBO"
sta2$stageSerialNb <- "20434532"

# Combine HOBO data into single data frame
sta <- rbind(sta, sta2)

# Read the sixth HOBO file
dfloc <- paste0(staloc, flist[6])
sta2 <- read.csv(dfloc, skip = 1)
sta2 <- sta2[2:4]
sta2 <- sta2 %>%
  rename(DateTime.EST = Date.Time..GMT.05.00,
         pres.water.kpa  = Abs.Pres..kPa..LGR.S.N..22063078..SEN.S.N..22063078.,
         temp.water.C = Temp...C..LGR.S.N..22063078..SEN.S.N..22063078.)
sta2$DateTime.EST <- as.POSIXct(sta2$DateTime.EST, tz = "EST",
                                format = "%m/%d/%y %I:%M:%S %p")
sta2 <- na.omit(sta2)
sta2$stageDevice <- "black HOBO"
sta2$stageSerialNb <- "22063078"

# Combine HOBO data into single data frame
sta <- rbind(sta, sta2)
sta <- sta[!duplicated(sta),]

# Merge baro and HOBO data
dat <- merge(barodat, sta, by = "DateTime.EST", all = FALSE)
dat <- dat[!duplicated(dat),]

# 3. Convert pressure to depth ------------------------------------------

# Conversion factors
FEET_TO_METERS = 0.3048
KPA_TO_PSI = 0.1450377
PSI_TO_PSF = 144.0

# water pressure
dat$pres.diff.kpa <- dat$pres.water.kpa - dat$pres.baro.kpa

# Fluid density calculation (from Reese LeVea)
dat$density.kgm3 = (999.83952 + (16.945176 * dat$temp.water.C) - 
                      (7.9870401e-03 * dat$temp.water.C**2) - 
                      (46.170461e-06 * dat$temp.water.C**3) + 
                      (105.56302e-09 * dat$temp.water.C**4) - 
                      (280.54253e-12 * dat$temp.water.C**5)) /
  (1 + 16.879850e-03 * dat$temp.water.C) # density in kg/m3

# Convert density from kg/m3 to lb/ft^3
dat$density.lbft <- 0.0624279606 * dat$density.kgm3


# Convert pressure to density-dependent fluid depth array
dat$stage.raw.m = (FEET_TO_METERS) * (KPA_TO_PSI * PSI_TO_PSF * dat$pres.diff.kpa) / dat$density.lbft
dat$stage.raw.m[(dat$stage.raw.m<0)] <- NA
dat[sapply(dat, is.infinite)] <- NA
dat <- subset(dat, select = -(density.lbft))

# Export data ----------------------------------------------------------
dat2 <- dat
dat$DateTime.EST <- as.character(format(dat$DateTime.EST))
write.csv(dat, paste0(dataloc,"/CLGB.UP/stage/y2024/finaldata/CLGB.UP_2024_waterDepths.csv"), row.names = F)
dat <- dat2


# Corrected stage ---------------------------------------------------------

offset2024A <- 0.015974643
offsetchangedateA <- as.POSIXct("2024-09-26 16:00")

offset2024B <- 0.028449803
offsetchangedateB <- as.POSIXct("2024-12-11 13:30")

offset2024C <- -0.066636168
offsetchangedateC <- as.POSIXct("2024-12-11 16:45")

for (i in 1:nrow(dat)){
  if (dat$DateTime.EST[i] < offsetchangedateA){
    dat$stage.correction.factor.m[i] = offset2024A
  } else if (dat$DateTime.EST[i] < offsetchangedateB){
    dat$stage.correction.factor.m[i] = offset2024B
  } else if (dat$DateTime.EST[i] < offsetchangedateC){
    dat$stage.correction.factor.m[i] = NA
  } else{
    dat$stage.correction.factor.m[i] = offset2024C
  }
}

dat$stage.corrected.m <- dat$stage.raw.m + dat$stage.correction.factor.m

# Discharge ---------------------------------------------------------------

dat$Q.m3s <- 3.4045  * dat$stage.corrected.m** 4.9984

for (i in 1:nrow(dat)){
  if (is.na(dat$stage.corrected.m[i])){
    dat$Q.m3sQF[i] <- 1
  } else if(dat$stage.corrected.m[i] > 0.66){
    dat$Q.m3sQF[i] <- 1
  } else{
    dat$Q.m3sQF[i] <- 0
  }
}


plot(dat$DateTime.EST, dat$Q.m3s, type = "l",
     ylab = "Q (m3/s)", xlab = "Date 2024")
plot(dat$DateTime.EST, dat$Q.m3s, type = "l", log = "y",
     ylab = "Q (m3/s)", xlab = "Date 2024")

dat2 <- dat
dat$DateTime.EST <- as.character(format(dat$DateTime.EST))
write.csv(dat, paste0(dataloc,"/CLGB.UP/ratingCurve/CLGB.UP_2024_DISCHARGE.csv"), row.names = F)
write.csv(dat, paste0(dataloc,"/CLGB.UP/stage/y2024/finalData/CLGB.UP_2024_DISCHARGE.csv"), row.names = F)
dat <- dat2


