# Temporary discharge data for CLGB.Up and CLGB.Ag


# Part 1: CLGB.Ag --------------------------------------------------

# Load relevant libraries
library(tidyverse)

# Set data locations

dataloc <- "C:/Users/laram/OneDrive - USNH/"

clgbAgloc <- paste(dataloc, "CLGB.AG/data/transformedData/y2024/EXO/EXO_CLGBag_2024-06-21-to-2024-07-22_pres.csv", sep = "")

agSt <- read.csv(clgbAgloc)
head(agSt)
agSt$DateTime <- paste(agSt$Date, agSt$Time)
agSt$DateTime <- as.POSIXct(agSt$DateTime, tz = "EST",
                            format = "%m/%d/%Y %H:%M:%S")
agSt$DateTime <- round_date(agSt$DateTime, unit="15 minutes")
agSt <- agSt[c("DateTime", "Pressure.psi", "Temp")]


# Right now the data is from Strawbery Banke - not ideal!
baroloc <- paste(dataloc, "CLGB.AG/data/rawData/y2024/Baro/temp_06-07-2024_baro.csv", sep = "")
baro <- read.csv(baroloc)
baro <- subset(baro, select = -c(station_sn, station_name, sensor_measurement_type, us_unit))
baro$DateTime <- as.POSIXct(baro$timestamp, tz = "EST",
                            format = "%Y-%m-%d %H:%M")
baro <- subset(baro, select = -c(timestamp))

# Merge barometric data with stage data
stage <- merge(agSt, baro, by = "DateTime", all.x = TRUE, all.y = FALSE)
stage$AbsPresSta.kPa <- stage$Pressure.psi*6.8947572932
stage$AbsPres.kPa <- stage$us_value*6.8947572932


stage$watPres.kPa <- stage$AbsPresSta.kPa - stage$AbsPres.kPa

# Fluid density calculation (from Reese LeVea)
stage$density = (999.83952 + (16.945176 * stage$Temp) - 
                   (7.9870401e-03 * stage$Temp**2) - 
                   (46.170461e-06 * stage$Temp**3) + 
                   (105.56302e-09 * stage$Temp**4) - 
                   (280.54253e-12 * stage$Temp**5)) /
  (1 + 16.879850e-03 * stage$Temp) # density in kg/m3

# Convert density from kg/m3 to lb/ft^3
stage$density.lbft <- 0.0624279606 * stage$density

# Conversion factors
FEET_TO_METERS = 0.3048
KPA_TO_PSI = 0.1450377
PSI_TO_PSF = 144.0

# Convert pressure to density-dependent fluid depth array

stage$depth.m = (FEET_TO_METERS) * (KPA_TO_PSI * PSI_TO_PSF * stage$watPres.kPa) / stage$density.lbft

saveloc <- paste(dataloc, "CLGB.AG/data/transformedData/y2024/EXO/CLGB_ag_EXO_depth.csv", sep = "")
write.csv(stage, saveloc, row.names = F)


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

saveloc <- paste(dataloc, "CLGB.AG/data/transformedData/CLGBag_DISCHARGE_2022-2023.csv", sep = "")
write.csv(stage, saveloc, row.names = F)



# Part 2: CLGB.Up --------------------------------------------------

# Set data locations

dataloc <- "C:/Users/laram/OneDrive - USNH/CLGB/"

clgbUploc <- paste0(dataloc, "CLGB.UP/rawData/y2024/Stage/")

staloc <- paste0(dataloc, "CLGB.AG/data/rawData/y2024/Stage/")
flist <- list.files(staloc, pattern = "*_silver.csv")
flist

agSt <- read.csv(clgbUploc)
head(agSt)
agSt$DateTime <- as.POSIXct(agSt$DateTime, tz = "EST",
                            format = "%m/%d/%y %I:%M:%S %p")
agSt$Temp <- agSt$Temp.C
agSt <- agSt[c("DateTime", "AbsPres.psi", "Temp")]


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
stage <- merge(agSt, baro, by = "DateTime", all.x = FALSE, all.y = FALSE)
stage$AbsPresSta.kPa <- stage$AbsPres.psi*6.8947572932
stage$AbsPres.kPa <- stage$us_value*6.8947572932


stage$watPres.kPa <- stage$AbsPresSta.kPa - stage$AbsPres.kPa

# Fluid density calculation (from Reese LeVea)
stage$density = (999.83952 + (16.945176 * stage$Temp) - 
                   (7.9870401e-03 * stage$Temp**2) - 
                   (46.170461e-06 * stage$Temp**3) + 
                   (105.56302e-09 * stage$Temp**4) - 
                   (280.54253e-12 * stage$Temp**5)) /
  (1 + 16.879850e-03 * stage$Temp) # density in kg/m3

# Convert density from kg/m3 to lb/ft^3
stage$density.lbft <- 0.0624279606 * stage$density

# Conversion factors
FEET_TO_METERS = 0.3048
KPA_TO_PSI = 0.1450377
PSI_TO_PSF = 144.0

# Convert pressure to density-dependent fluid depth array

stage$depth.m = (FEET_TO_METERS) * (KPA_TO_PSI * PSI_TO_PSF * stage$watPres.kPa) / stage$density.lbft

saveloc <- paste(dataloc, "CLGB.UP/Stage/CLGB_up_stage_depth.csv", sep = "")
write.csv(stage, saveloc, row.names = F)


# Covert depth to density
# Rating curve is in its own Excel file
# This equation needs to be changed for each site based on the rating curve

stage$discharge.m3s <- 1.5951*(stage$depth.m^4.3296)

# Remane columns with awkward names

stage <- stage %>%
  rename( TempWat.C.stage = Temp.C.x,
          TempAir.C = Temp.C.y,
          AbsPresAir.kPa = AbsPres.kPa,
          AbsPresWat.psi = AbsPres.psi)

stage$DateTime <- as.character(format(stage$DateTime))

saveloc <- paste(dataloc, "CLGB.AG/data/transformedData/CLGBag_DISCHARGE_2022-2023.csv", sep = "")
write.csv(stage, saveloc, row.names = F)
