## CLGB_AG Data Analysis
## Created 2023-11
## Lara Munro


# Part 0: set up R space --------------------------------------------------

# Load relevant libraries
library(tidyverse)

# Set data locations

dataloc <- "C:/Users/laram/OneDrive - USNH/CLGB/"

yr <- 2022

# HOBO stage ------------------------------------------------------

# Convert HOBO stage files using HOBOware and aggregate the files
# Using a TEXT EDITOR (NOT EXCEL!! - it has trouble with dates), update the column names

# Read tables from 2022
yr <- 2022
staloc <- paste(dataloc, "transformedData/y", yr, "/Stage/", sep = "")
flist <- list.files(staloc, pattern = "*.csv")
flist

dfloc <- paste(staloc, flist[1], sep ="")
sta <- read.csv(dfloc)
sta <- subset(sta, select = -c(row.nb))
sta$DateTime <- as.POSIXct(sta$DateTime, tz = "EST",
                           format = "%m/%d/%y %I:%M:%S %p")

# Read tables from 2023
yr <- 2023
staloc <- paste(dataloc, "transformedData/y", yr, "/Stage/", sep = "")
flist <- list.files(staloc, pattern = "*.csv")
flist

dfloc <- paste(staloc, flist[1], sep ="")
sta2 <- read.csv(dfloc)
sta2 <- subset(sta2, select = -c(row.nb))
sta2$DateTime <- as.POSIXct(sta2$DateTime, tz = "EST",
                            format = "%m/%d/%y %I:%M:%S %p")

# Merge the 2022 and 2023 datasets together
sta <- rbind(sta, sta2)


# Read annual barometric data ---------------------------------------------


# Read barometrice pressure data
# Get this data from Jody Potter at WQAL, make sure that units are the same 
# This data includes 2022 and 2023 in the same data file. It has been included in the 2022 and 2023 baro folder.
baroloc <- paste(dataloc, "WHB Barometric/annual/WHB_atm_Pressure2024.csv", sep = "")
baro <- read.csv(baroloc)
baro <- subset(baro, select = -c(rownb))
baro2024$DateTime <- as.POSIXct(baro2024$datetime, tz = "EST",
                            format = "%Y-%m-%d %H:%M")

# Merge barometric data with stage data
stage <- merge(sta, baro2024, by = "DateTime",  all.x = TRUE, all.y = FALSE)

stage$watPres.kPa <- stage$pres.kpa.x - stage$pres.kpa.y

# Fluid density calculation (from Reese LeVea)
stage$density = (999.83952 + (16.945176 * stage$temp.C.x) - 
                   (7.9870401e-03 * stage$temp.C.x**2) - 
                   (46.170461e-06 * stage$temp.C.x**3) + 
                   (105.56302e-09 * stage$temp.C.x**4) - 
                   (280.54253e-12 * stage$temp.C.x**5)) /
  (1 + 16.879850e-03 * stage$temp.C.x) # density in kg/m3

# Convert density from kg/m3 to lb/ft^3
stage$density.lbft <- 0.0624279606 * stage$density

# Conversion factors
FEET_TO_METERS = 0.3048
KPA_TO_PSI = 0.1450377
PSI_TO_PSF = 144.0

# Convert pressure to density-dependent fluid depth array

stage$depth.m = (FEET_TO_METERS) * (KPA_TO_PSI * PSI_TO_PSF * stage$watPres.kPa) / stage$density.lbft

write.csv(stage, paste0(dataloc,"/CLGB.AG/data/ratingCurve/CLGB.AG_2024_waterDepth.csv"), row.names = F)

# Covert depth to density
# Rating curve is in its own Excel file
# This equation needs to be changed for each site based on the rating curve

stage$discharge.m3s <- 0.0478*(stage$depth.m^1.7148)

# Remane columns with awkward names

stage <- stage %>%
  rename( TempWat.C.stage = Temp.C.x,
          TempAir.C = Temp.C.y,
          AbsPresAir.kPa = AbsPres.kPa,
          AbsPresWat.psi = AbsPres.psi)

stage$DateTime <- as.character(format(stage$DateTime))

saveloc <- paste(dataloc, "transformedData/CLGBag_DISCHARGE_2022-2023.csv", sep = "")
write.csv(stage, saveloc, row.names = F)


# plot discharge
for (i in 1:nrow(stage)){
  if (!is.na(stage$discharge.m3s[i])){
    if (stage$discharge.m3s[i] < 0.001){
      stage$discharge.m3s[i] <- NA
    }
  }
}

stage2 <- na.omit(stage)

par(mar = c(4.5, 5, 2, 2))
plot(stage2$DateTime, stage2$discharge.m3s, type = "l",
     log = "y",
     xlab = "Date",
     ylab = expression(paste("Discharge (m"^"3","/s)")),
     col = "darkslateblue",
     cex.lab = 1.5,
     cex.axis = 1.5,
     yaxt = "n"
)
axis(side = 2, at = c(0.01, 0.1, 1), cex.axis = 1.5) 	 


