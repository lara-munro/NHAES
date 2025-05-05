# HOBO data aggregation


# Load relevant libraries
library(tidyverse)

# Set data locations

dataloc <- "C:/Users/laram/OneDrive - USNH/CLGB/"


# CLGB.Ag stage -----------------------------------------------------------

# Read tables from 2024 for black body HOBO
staloc <- paste0(dataloc, "CLGB.AG/data/rawData/y2024/Stage/")
flist <- list.files(staloc, pattern = "*_black.csv")
flist

dfloc <- paste0(staloc, flist[1])
sta <- read.csv(dfloc, skip = 1)
sta <- sta[2:4]
sta <- sta %>%
  rename(DateTime = Date.Time..GMT.05.00,
         pres.kpa = Abs.Pres..kPa..LGR.S.N..22063078..SEN.S.N..22063078.,
         temp.C = Temp...C..LGR.S.N..22063078..SEN.S.N..22063078.)
sta$DateTime <- as.POSIXct(sta$DateTime, tz = "EST",
                           format = "%m/%d/%y %I:%M:%S %p")
sta <- na.omit(sta)

dfloc <- paste0(staloc, flist[2])
sta2 <- read.csv(dfloc, skip = 1)
sta2 <- sta2[2:4]
sta2 <- sta2 %>%
  rename(DateTime = Date.Time..GMT.05.00,
         pres.kpa = Abs.Pres..kPa..LGR.S.N..22063078_duplicate..SEN.S.N..22063078.,
         temp.C = Temp...C..LGR.S.N..22063078_duplicate..SEN.S.N..22063078.)
sta2$DateTime <- as.POSIXct(sta2$DateTime, tz = "EST",
                            format = "%m/%d/%y %I:%M:%S %p")
sta2 <- na.omit(sta2)


# merge data frames

sta <- rbind(sta, sta2)

# Remove duplicated rows
sta <- sta[!duplicated(sta),]

write.csv(sta, paste0(dataloc,"/CLGB.AG/data/transformedData/y2024/CLGB.Ag_waterPres.csv"))



# CLGB.Up stage -----------------------------------------------------------

# Read tables from 2024 for black body HOBO
staloc <- paste0(dataloc, "CLGB.UP/rawData/y2024/Stage/")
flist <- list.files(staloc, pattern = "*.csv")
flist

dfloc <- paste0(staloc, flist[1])
sta <- read.csv(dfloc, skip = 1)
sta <- sta[2:4]
sta <- sta %>%
  rename(DateTime = Date.Time..GMT.05.00,
         pres.psi = Abs.Pres..psi..LGR.S.N..20434532..SEN.S.N..20434532.,
         temp.C = Temp...C..LGR.S.N..20434532..SEN.S.N..20434532.)
sta$pres.kpa <- 6.89476 * sta$pres.psi
sta$DateTime <- as.POSIXct(sta$DateTime, tz = "EST",
                           format = "%m/%d/%y %I:%M:%S %p")
sta <- na.omit(sta)

dfloc <- paste0(staloc, flist[2])
sta2 <- read.csv(dfloc, skip = 1)
sta2 <- sta2[2:4]
sta2 <- sta2 %>%
  rename(DateTime = Date.Time..GMT.05.00,
         pres.psi = Abs.Pres..psi..LGR.S.N..20434532..SEN.S.N..20434532.,
         temp.C = Temp...C..LGR.S.N..20434532..SEN.S.N..20434532.)
sta2$pres.kpa <- 6.89476 * sta2$pres.psi
sta2$DateTime <- as.POSIXct(sta2$DateTime, tz = "EST",
                            format = "%m/%d/%y %I:%M:%S %p")
sta2 <- na.omit(sta2)

dfloc <- paste0(staloc, flist[3])
sta3 <- read.csv(dfloc, skip = 1)
sta3 <- sta3[2:4]
sta3 <- sta3 %>%
  rename(DateTime = Date.Time..GMT.05.00,
         pres.psi = Abs.Pres..psi..LGR.S.N..20434532..SEN.S.N..20434532.,
         temp.C = Temp...C..LGR.S.N..20434532..SEN.S.N..20434532.)
sta3$pres.kpa <- 6.89476 * sta3$pres.psi
sta3$DateTime <- as.POSIXct(sta3$DateTime, tz = "EST",
                            format = "%m/%d/%y %I:%M:%S %p")
sta3 <- na.omit(sta3)

# merge data frames

sta <- rbind(sta, sta3)

# Remove duplicated rows
sta <- sta[!duplicated(sta),]

write.csv(sta, paste0(dataloc,"CLGB.UP/transformedData/y2024/CLGB.UP_waterPres.csv"))




# Aggregate barometric data -----------------------------------------------
#Aggregate barometric data (not necessarily needed)
partbaroloc <- paste(dataloc, "WHB Barometric/", sep = "")
baro2024 <- read.csv(paste(partbaroloc, "annual/WHB_atm_Pressure2024.csv", sep = ""))
baro2024$DateTime <- as.POSIXct(baro2024$DateTime, tz = "EST",
                             format = "%Y-%m-%d %H:%M")
baro2024 <- na.omit(baro2024)
baro2024 <- baro2024[3:5]

part1 <- read.csv(paste0(partbaroloc, "WHB_B_20240301to20240507.csv"), skip = 1)
part1$DateTime <- as.POSIXct(part1$datetime, tz = "EST",
                                format = "%m/%d/%y %I:%M:%S %p")
part1 <- part1[c(3:4, 9)]

part2 <- read.csv(paste0(partbaroloc, "WHB_B_20240507to20240626.csv"), skip = 1)
part2 <- part2 %>%
  rename(rownb = X.,
         datetime = Date.Time..GMT.05.00,
         pres.kpa = Abs.Pres..kPa..LGR.S.N..10012043..SEN.S.N..10012043.,
         temp.C = Temp...C..LGR.S.N..10012043..SEN.S.N..10012043.)
part2$DateTime <- as.POSIXct(part2$datetime, tz = "EST",
                             format = "%m/%d/%y %I:%M:%S %p")
part2 <- part2[c(3:4, 9)]

part3 <- read.csv(paste0(partbaroloc, "WHB_B_20240626to20240825.csv"), skip = 1)
part3 <- part3[1:4]
part3 <- part3 %>%
  rename(rownb = X.,
         datetime = Date.Time..GMT.05.00,
         pres.psi = Abs.Pres..psi..LGR.S.N..10012043..SEN.S.N..10012043.,
         temp.F = Temp...F..LGR.S.N..10012043..SEN.S.N..10012043.)
part3$pres.kpa <- 6.89476 * part3$pres.psi
part3$temp.C <- (part3$temp.F - 32) *5/9
part3$DateTime <- as.POSIXct(part3$datetime, tz = "EST",
                             format = "%m/%d/%y %I:%M:%S %p")

part3 <- part3[5:7]

part4 <- read.csv(paste0(partbaroloc, "WHB_B_20240815to20240924.csv"), skip = 1)
part4 <- part4[1:4]
part4 <- part4 %>%
  rename(rownb = X.,
         datetime = Date.Time..GMT.05.00,
         pres.psi = Abs.Pres..psi..LGR.S.N..10012043..SEN.S.N..10012043.,
         temp.F = Temp...F..LGR.S.N..10012043..SEN.S.N..10012043.)
part4$pres.kpa <- 6.89476 * part4$pres.psi
part4$temp.C <- (part4$temp.F - 32) *5/9
part4$DateTime <- as.POSIXct(part4$datetime, tz = "EST",
                             format = "%m/%d/%y %I:%M:%S %p")
part4 <- part4[5:7]

baro2024 <- rbind(baro2024, part4)
baro2024 <- baro2024[!duplicated(baro2024),]


part5 <- read.csv(paste0(partbaroloc, "WHB_B_20240924to20241105.csv"), skip = 1)
part5 <- part5[1:4]
part5 <- part5 %>%
  rename(rownb = X.,
         datetime = Date.Time..GMT.05.00,
         pres.psi = Abs.Pres..psi..LGR.S.N..10012043..SEN.S.N..10012043.,
         temp.F = Temp...F..LGR.S.N..10012043..SEN.S.N..10012043.)
part5$pres.kpa <- 6.89476 * part5$pres.psi
part5$temp.C <- (part5$temp.F - 32) *5/9
part5$DateTime <- as.POSIXct(part5$datetime, tz = "EST",
                             format = "%m/%d/%y %I:%M:%S %p")
part5 <- part5[5:7]


baro2024 <- rbind(baro2024, part5)
baro2024 <- baro2024[!duplicated(baro2024),]

part6 <- read.csv(paste0(partbaroloc, "WHB_B_20241105to20241219.csv"), skip = 1)
head(part6)
part6 <- part6[1:4]
part6 <- part6 %>%
  rename(rownb = X.,
         datetime = Date.Time..GMT.05.00,
         pres.psi = Abs.Pres..psi..LGR.S.N..10012043..SEN.S.N..10012043.,
         temp.F = Temp...F..LGR.S.N..10012043..SEN.S.N..10012043.)
part6$pres.kpa <- 6.89476 * part6$pres.psi
part6$temp.C <- (part6$temp.F - 32) *5/9
part6$DateTime <- as.POSIXct(part6$datetime, tz = "EST",
                             format = "%m/%d/%y %I:%M:%S %p")
part6 <- part6[5:7]

baro2024 <- rbind(baro2024, part6)
baro2024 <- baro2024[!duplicated(baro2024),]


part7 <- read.csv(paste0(partbaroloc, "WHB_B_20241219to20250204.csv"), skip = 1)
head(part7)
part7 <- part7[1:4]
part7 <- part7 %>%
  rename(rownb = X.,
         datetime = Date.Time..GMT.05.00,
         pres.psi = Abs.Pres..psi..LGR.S.N..10012043..SEN.S.N..10012043.,
         temp.F = Temp...F..LGR.S.N..10012043..SEN.S.N..10012043.)
part7$pres.kpa <- 6.89476 * part7$pres.psi
part7$temp.C <- (part7$temp.F - 32) *5/9
part7$DateTime <- as.POSIXct(part7$datetime, tz = "EST",
                             format = "%m/%d/%y %I:%M:%S %p")
part7 <- part7[5:7]

baro2024 <- rbind(baro2024, part7)
baro2024 <- baro2024[!duplicated(baro2024),]


baro2024$DateTime <- as.character(format(baro2024$DateTime))
write.csv(baro2024, paste0(partbaroloc, "annual/WHB_atm_Pressure2024.csv"), row.names = F)


# Read annual barometric data ---------------------------------------------


# Read barometrice pressure data
# Get this data from Jody Potter at WQAL, make sure that units are the same 
# This data includes 2022 and 2023 in the same data file. It has been included in the 2022 and 2023 baro folder.
baroloc <- paste(dataloc, "WHB Barometric/annual/WHB_atm_Pressure2024.csv", sep = "")
baro <- read.csv(baroloc)
baro$DateTime <- as.POSIXct(baro$DateTime, tz = "EST",
                                format = "%Y-%m-%d %H:%M")

# Merge barometric data with stage data
stage <- merge(sta, baro, by = "DateTime",  all.x = TRUE, all.y = FALSE)
stage <- stage[!duplicated(stage),]
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

stage$DateTime <- as.character(format(stage$DateTime))
write.csv(stage, paste0(dataloc,"/CLGB.UP/ratingCurve/CLGB.UP_2024_waterDepth.csv"), row.names = F)

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

