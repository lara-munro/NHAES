## CLGB_AG Data Analysis
## Created 2023-11
## Lara Munro


# Part 0: set up R space --------------------------------------------------

# Load relevant libraries
library(tidyverse)
library(ggplot2)

# Set data locations

dataloc <- "C:/Users/laram/OneDrive - USNH/CLGB.AG/data/"

yr <- 2022

# Part 1: Conductivity ----------------------------------------------------

# Convert HOBO conductivity files using HOBOware and aggregate the files
# Using a TEXT EDITOR (NOT EXCEL!! - it has trouble with dates), update the column names

yr <- 2022
condloc <- paste(dataloc, "transformedData/y", yr, "/Conductivity/", sep = "")
flist <- list.files(condloc, pattern = "*.csv")
flist

cond <- read.csv((paste(condloc, flist[1], sep = "")))
cond <- subset(cond, select = -c(row.nb))
cond$DateTime <- as.POSIXct(cond$DateTime, tz = "EST",
                          format = "%m/%d/%y %I:%M:%S %p")

yr <- 2023
condloc <- paste(dataloc, "transformedData/y", yr, "/Conductivity/", sep = "")
flist <- list.files(condloc, pattern = "*.csv")
flist

cond2 <- read.csv((paste(condloc, flist[1], sep = "")))
cond2 <- subset(cond2, select = -c(row.nb))
cond2$DateTime <- as.POSIXct(cond2$DateTime, tz = "EST",
                            format = "%m/%d/%y %I:%M:%S %p")

cond <- rbind(cond, cond2)
# Remove duplicated rows
cond <- cond[!duplicated(cond),]

# Rename columns
cond <- cond %>%
  rename(TempWat.C.cond = Temp.C)

cond$DateTime <- as.character(format(cond$DateTime))

# write csv 
saveloc <- paste(dataloc, "transformedData/CLGBag_COND_2022-2023.csv", sep = "")
write.csv(cond, saveloc, row.names = F)


#par(mfrow=c(1,1))
plot(cond$DateTime, cond$Cond.uScm, type = "l",
     xlab = "Date",
     ylab = "Conductivity (uS/cm)")
plot(cond$DateTime, cond$Temp.C.Stage, type = "l",
     xlab = "Date",
     ylab = "Temperature (C)")

# Part 2: Turner ----------------------------------------------------------
yr <- 2022
tuloc <- paste(dataloc, "rawData/y2022/Turner/CLGB_AG_Turner_2022-06-10-to-2022-11-28.csv", sep = "")
tur <- read.csv(tuloc)
tur$DateTime <- as.POSIXct(tur$DateTime, tz = "EST",
                          format = "%m/%d/%Y %H:%M")

# plot data
plot(tur$DateTime, tur$CDOM.ppb, type = "l", 
     xlab = "Date 2022", ylab = "CDOM (ppb)")
plot(tur$DateTime, tur$Turbidity.NTU, type = "l", 
     xlab = "Date 2022", ylab = "Turbidity (NTU)")
plot(tur$DateTime, tur$Rhodamine.ppb, type = "l", 
     xlab = "Date 2022", ylab = "Rhodamine (ppb)")
plot(tur$DateTime, tur$Temp.C, type = "l", 
     xlab = "Date 2022", ylab = "Temperature (C)")

tuloc <- paste(dataloc, "transformedData/y2023/Turner/CLGB_AG_2023-07-18-to-2023-08-04.csv", sep = "")
tur2 <- read.csv(tuloc)
tur2$DateTime <- as.POSIXct(tur2$DateTime, tz = "EST",
                           format = "%m/%d/%y %H:%M:%S")

alltur <- rbind(tur, tur2)
alltur$DateTime <- as.character(format(alltur$DateTime))

saveloc <- paste(dataloc, "transformedData/CLGBag_Turner_2022-2023.csv", sep = "")
write.csv(alltur, saveloc, row.names = F)

# Part 3: SUNA ------------------------------------------------------------
yr <- 2023
sunaloc <- paste(dataloc, "transformedData/y", yr, "/SUNA/", sep = "")

flist <- list.files(sunaloc, pattern = "*.csv")
flist

suna <- read.csv((paste(sunaloc, flist[2], sep = "")))

sunaloc <- paste(dataloc, "rawData/y2022/SUNA/SUNA_WQual_2022-06-06-to-2022-11-28.csv", sep = "")
suna <- read.csv(sunaloc)


# Create a single datetime column
suna$date <- paste(suna$Year, suna$Month, suna$DOM, sep = "-")  
for (i in 1:nrow(suna)){
  if (suna$Minute[i] == 0){
    suna$Minute[i] <- "00"
  }
  if (suna$Hour[i] == 0){
    suna$Hour[i] <- "00"
  }
}
suna$time <- paste(suna$Hour, suna$Minute, sep = ":") 

suna$DateTime <- paste(suna$date, suna$time)
suna$DateTime <- as.POSIXct(suna$DateTime, tz = "EST")

# Remove extra data

suna <- subset(suna, select = c("DateTime", "Nitrate_mg", "SUNA_Error"))
#suna$Nitrate_mg <- as.numeric(suna$Nitrate_mg)
#suna$SUNA_Error <- as.numeric(suna$SUNA_Error)
suna <- suna %>% mutate(SUNA_Flag = ifelse(SUNA_Error > 0.05, 1, 0))

# remove flagged values
for (i in 1:nrow(suna)){
  if (suna$SUNA_Error[i] != 0){
    suna$Nitrate_mg[i] <- NA
  }
}

# save the file with SUNA data
suna$DateTime <- as.character(format(suna$DateTime))

saveloc <- paste(dataloc, "transformedData/CLGBag_SUNA_2023.csv", sep = "")
write.csv(suna, saveloc, row.names = F)


# Part 4: HOBO stage ------------------------------------------------------

# Convert HOBO stage files using HOBOware and aggregate the files
# Using a TEXT EDITOR (NOT EXCEL!! - it has trouble with dates), update the column names

yr <- 2022
staloc <- paste(dataloc, "transformedData/y", yr, "/Stage/", sep = "")
flist <- list.files(staloc, pattern = "*.csv")
flist

dfloc <- paste(staloc, flist[1], sep ="")
sta <- read.csv(dfloc)
sta <- subset(sta, select = -c(row.nb))
sta$DateTime <- as.POSIXct(sta$DateTime, tz = "EST",
                          format = "%m/%d/%y %I:%M:%S %p")

yr <- 2023
staloc <- paste(dataloc, "transformedData/y", yr, "/Stage/", sep = "")
flist <- list.files(staloc, pattern = "*.csv")
flist

dfloc <- paste(staloc, flist[2], sep ="")
sta2 <- read.csv(dfloc)
sta2 <- subset(sta2, select = -c(row.nb))
sta2$DateTime <- as.POSIXct(sta2$DateTime, tz = "EST",
                           format = "%m/%d/%y %I:%M:%S %p")

# Merge the datasets together
sta <- rbind(sta, sta2)

# Remove duplicated rows
sta <- sta[!duplicated(sta),]

sta$AbsPresSta.kPa <- sta$AbsPres.psi*6.8947572932


# Read barometrice pressure data
# Get this data from Jody Potter at WQAL, make sure that units are the same 
# This data includes 2022 and 2023 in the same data file. It has been included in the 2022 and 2023 baro folder.
baroloc <- paste(dataloc, "transformedData/y", yr, "/Baro/WHB_atm_Pressure_2022to2023.csv", sep = "")
baro <- read.csv(baroloc)
baro <- subset(baro, select = -c(row.nb))
baro$DateTime <- as.POSIXct(baro$DateTime, tz = "EST",
                            format = "%Y-%m-%d %H:%M")

# Merge barometric data with stage data
stage <- merge(sta, baro, by = "DateTime", all.x = TRUE, all.y = FALSE)

stage$watPres.kPa <- stage$AbsPresSta.kPa - stage$AbsPres.kPa

# Fluid density calculation (from Reese LeVea)
stage$density = (999.83952 + (16.945176 * stage$Temp.C.x) - 
                   (7.9870401e-03 * stage$Temp.C.x**2) - 
                   (46.170461e-06 * stage$Temp.C.x**3) + 
                   (105.56302e-09 * stage$Temp.C.x**4) - 
                   (280.54253e-12 * stage$Temp.C.x**5)) /
  (1 + 16.879850e-03 * stage$Temp.C.x)

# Convert density to lb/ft^3
stage$density.lbft <- 0.0624279606 * stage$density

# Conversion factors
FEET_TO_METERS = 0.3048
KPA_TO_PSI = 0.1450377
PSI_TO_PSF = 144.0

# Convert pressure to density-dependent fluid depth array
stage$depth.m = FEET_TO_METERS * (KPA_TO_PSI * PSI_TO_PSF * stage$watPres.kPa) / stage$density.lbft

# Covert depth to density
# Rating curve is in its own Excel file

stage$discharge.m3s <- 0.3861*(stage$depth.m^3.4147)

# Remane columns with awkward names

stage <- stage %>%
rename( TempWat.C.stage = Temp.C.x,
        TempAir.C = Temp.C.y,
        AbsPresAir.kPa = AbsPres.kPa,
        AbsPresWat.psi = AbsPres.psi)

stage$DateTime <- as.character(format(stage$DateTime))

saveloc <- paste(dataloc, "transformedData/CLGBag_DISCHARGE_2022-2023.csv", sep = "")
write.csv(stage, saveloc, row.names = F)
  

# Stage if more than one synthesis file (i.e. duplicate datetimes) --------
sta <- data.frame()

staloc <- paste(dataloc, "transformedData/y", yr, "/Stage/", sep = "")

flist <- list.files(staloc, pattern = "*.csv")
flist

for (i in 1:length(flist)){
  print(i)
  dfloc <- paste(staloc, flist[i], sep ="")
  df <- read.csv(dfloc)
  df$DateTime <- as.POSIXct(df$DateTime, tz = "EST",
                            format = "%m/%d/%y %I:%M:%S %p")
  df <- subset(df, select = -c(row.nb))
  sta <- rbind(sta, df)
}



#par(mfrow=c(1,1))
plot(sta$DateTime, sta$AbsPres.psi, type = "l",
     xlab = "Date",
     ylab = "Pressure (psi)")
plot(sta$DateTime, sta$Temp.C, type = "l",
     xlab = "Date",
     ylab = "Temperature (C)")




vocabulary[duplicated(vocabulary$id),]

fart <- sta[(duplicated(sta$DateTime)),]


