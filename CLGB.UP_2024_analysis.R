# CLGB.Upper data analysis 2024
# Date: January 2025
# Author: Lara Munro

# This script was used to produce figures and data for the 2024 AES annual report. 
# As such, this script is a mishmash of code and data. 
# Most of the data analysis elements contained in this script can be found in more ordered code.

# 1. Set up R space -------------------------------------------------------
library(ggplot2)
library(tidyverse)

# Set data locations

dataloc <- "C:/Users/laram/OneDrive - USNH/CLGB/"

# 2. Calculate discharge ---------------------------------------------------

uploc <- paste0(dataloc, "CLGB.UP/stage/y2024/rawData")

flist <- list.files(uploc, pattern = "*.csv")
flist
stage<-do.call(rbind, lapply(paste0(dataloc, "CLGB.UP/stage/y2024/rawData/", flist),
                            read.csv, skip=1, header=FALSE))

stage$DateTime <- stage$V2
stage$pres.kpa <- stage$V3
stage$temp.C <- stage$V4
stage <- stage[-c(1), c("DateTime", "pres.kpa", "temp.C")]

stage$DateTime <- as.POSIXct(stage$DateTime, tz = "EST",
                            format = "%m/%d/%y %I:%M:%S %p")


# Read barometrice pressure data
# Get this data from Jody Potter at WQAL, make sure that units are the same 
# This data includes 2022 and 2023 in the same data file. It has been included in the 2022 and 2023 baro folder.
baroloc <- paste(dataloc, "WHB Barometric/annual/WHB_atm_Pressure2024.csv", sep = "")
dat <- read.csv(baroloc)
dat$DateTime <- as.POSIXct(dat$DateTime, tz = "EST",
                           format = "%Y-%m-%d %H:%M")
dat <- dat %>%
  rename(pres.kpa.baro = pres.kpa,
         temp.C.baro = temp.C)
dat <- na.omit(dat)

# Merge barometric data with stage data
stage <- merge(stage, dat, by = "DateTime", all.x = TRUE, all.y = FALSE)
stage$pres.kpa <- as.numeric(stage$pres.kpa)
stage$temp.C <- as.numeric(stage$temp.C)

stage$watPres.kPa <- stage$pres.kpa - stage$pres.kpa.baro

# Fluid density calculation (from Reese LeVea)
stage$density = (999.83952 + (16.945176 * stage$temp.C) - 
                   (7.9870401e-03 * stage$temp.C**2) - 
                   (46.170461e-06 * stage$temp.C**3) + 
                   (105.56302e-09 * stage$temp.C**4) - 
                   (280.54253e-12 * stage$temp.C**5)) /
  (1 + 16.879850e-03 * stage$temp.C) # density in kg/m3

# Convert density from kg/m3 to lb/ft^3
stage$density.lbft <- 0.0624279606 * stage$density

# Conversion factors
FEET_TO_METERS = 0.3048
KPA_TO_PSI = 0.1450377
PSI_TO_PSF = 144.0

# Convert pressure to density-dependent fluid depth array

stage$depth.m = (FEET_TO_METERS) * (KPA_TO_PSI * PSI_TO_PSF * stage$watPres.kPa) / stage$density.lbft

saveloc <- paste(dataloc, "CLGB.UP/ratingCurve/CLGB_up_waterDepths_2024.csv", sep = "")
stage2 <- stage
stage2$DateTime <- as.character(format(stage2$DateTime))
write.csv(stage2, saveloc, row.names = F)


# Covert depth to discharge
# Rating curve is in its own Excel file
# This equation needs to be changed for each site based on the rating curve

# Offset value calculated in the rating curve sheet
stage$depth.standardized.m <- stage$depth.m + 0.022991373

stage$discharge.m3s <- 3.0539*(stage$depth.standardized.m^5.7448)

stage2 <- stage
stage2$DateTime <- as.character(format(stage2$DateTime))

saveloc <- paste(dataloc, "CLGB.UP/stage/y2024/finalData/CLGB.UP_DISCHARGE_2024.csv", sep = "")
write.csv(stage2, saveloc, row.names = F)



# 3. Load and aggregate conductivity data ---------------------------------

condloc <- paste0(dataloc, "CLGB.UP/conductivity/y2024/rawData/")
flist <- list.files(condloc, pattern = "*.csv")
flist

cond<-do.call(rbind, lapply(paste0(condloc, flist),
                             read.csv, skip=1, header=FALSE))

cond$DateTime <- cond$V2
cond$cond.uScm <- cond$V3
cond$temp.C <- cond$V4
cond <- cond[-c(1), c("DateTime", "cond.uScm", "temp.C")]
cond$cond.uScm <- as.numeric(cond$cond.uScm)
cond$temp.C <- as.numeric(cond$temp.C)

cond$DateTime <- as.POSIXct(cond$DateTime, tz = "EST",
                             format = "%m/%d/%y %I:%M:%S %p")
cond2 <- cond
cond2$DateTime <- as.character(format(cond2$DateTime))

saveloc <- paste(dataloc, "CLGB.UP/conductivity/y2024/finalData/CLGB.UP_CONDUCTIVITY_2024.csv", sep = "")
write.csv(cond2, saveloc, row.names = F)

# Merge discharge and conductivity
dat <- merge(stage, cond, by = "DateTime")

saveloc <- paste(dataloc, "CLGB.UP/aggregated data/CLGB.UP_sensor_2024.csv", sep = "")
dat2 <- dat
dat2$DateTime <- as.character(format(dat2$DateTime))
write.csv(dat2, saveloc, row.names = F)


# 4. Add grab sample data ---------------------------------------------------
# Aggregate chemistry data in the grab sample aggregation code

# Subset CLGB.Upper data to compare with sensor
clgbup.chem <- chemdat[which(chemdat$site == "CLGB.Upper"),]

dat <- merge(dat, clgbup.chem, by = "DateTime", all.x = TRUE)

saveloc <- paste(dataloc, "CLGB.UP/aggregated data/CLGB.UP_2024.csv", sep = "")
dat2 <- dat
dat2$DateTime <- as.character(format(dat2$DateTime))
write.csv(dat2, saveloc, row.names = F)

# 4. Plot discharge and conductivity data ---------------------------------


# Conductivity and discharge

par(mar = c(4, 5, 2, 4) + 0.3)			 
plot(dat$DateTime, dat$cond.uScm, type = "l",
     xlab = "Date",
     ylab = "Specific conductance (uS/cm)",
     col = "goldenrod",
     cex.lab = 1.5,
     cex.axis = 1.5
)
# set parameter new=True for a new axis 
par(new = TRUE)		 
# Draw second plot using axis y2 
plot(dat$DateTime, dat$discharge.m3s, type = "l", 
     log = "y",
     col = "darkslateblue", 
     axes = FALSE, 
     xlab = "", 
     ylab = "") 
axis(side = 4, at = c(0.01, 0.1, 1), cex.axis = 1.5) 	 
mtext(expression(paste("Discharge (m"^"3","/s)")), side = 4, line = 3, cex = 1.5)
