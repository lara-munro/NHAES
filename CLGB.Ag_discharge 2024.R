# CLGB.Ag depth aggregation
# Aggregate pressure and depth data for CLGB.Ag from 2 HOBOs and 1 YSI EXO
# Done for 2024 because of absent HOBOs for part of the year and switch between 2 HOBOs and stilling wells

# Creator: Lara Munro
# Date: December 2 2024


# Load libraries
library(tidyverse)
library(dplyr)

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
baroloc <- paste(dataloc, "WHB Barometric/annual/WHB_atm_Pressure2024.csv", sep = "")
barodat <- read.csv(baroloc)
barodat$DateTime <- as.POSIXct(barodat$DateTime, tz = "EST",
                               format = "%Y-%m-%d %H:%M")
barodat <- barodat %>%
  rename(DateTime.EST = DateTime,
         pres.baro.kpa = pres.kpa,
         temp.baro.C = temp.C)
barodat <- na.omit(barodat)

# 2. Read silver body HOBO ------------------------------------------------

# Read tables from 2024 for silver HOBO
staloc <- paste0(dataloc, "CLGB.AG/data/stage/y2024/rawData/")
flist <- list.files(staloc, pattern = "*_silver.csv")
flist

# Read the first silver HOBO file
dfloc <- paste0(staloc, flist[1])
sta <- read.csv(dfloc, skip = 1)
sta <- sta[2:4]
sta <- sta %>%
  rename(DateTime.EST = Date.Time..GMT.05.00,
         pres.water.kpa  = Abs.Pres..kPa..LGR.S.N..1303192..SEN.S.N..1303192.,
         temp.water.C = Temp...C..LGR.S.N..1303192..SEN.S.N..1303192.)
sta$DateTime.EST <- as.POSIXct(sta$DateTime.EST, tz = "EST",
                           format = "%m/%d/%y %I:%M:%S %p")
sta <- na.omit(sta)
sta$stageDevice <- "silver HOBO"
sta$stageSerialNb <- "1303192"

# Read the second silver HOBO file
dfloc <- paste0(staloc, flist[2])
sta2 <- read.csv(dfloc, skip = 1)
sta2 <- sta2[2:4]
sta2 <- sta2 %>%
  rename(DateTime.EST = Date.Time..GMT.05.00,
         pres.water.kpa  = Abs.Pres..kPa..LGR.S.N..1303192..SEN.S.N..1303192.,
         temp.water.C = Temp...C..LGR.S.N..1303192..SEN.S.N..1303192.)
sta2$DateTime.EST <- as.POSIXct(sta2$DateTime.EST, tz = "EST",
                               format = "%m/%d/%y %I:%M:%S %p")
sta2 <- na.omit(sta2)
sta2$stageDevice <- "silver HOBO"
sta2$stageSerialNb <- "1303192"

# Combine HOBO data into single data frame
sta <- rbind(sta, sta2)

# Read the third silver HOBO file
dfloc <- paste0(staloc, flist[3])
sta2 <- read.csv(dfloc, skip = 1)
sta2 <- sta2[2:4]
sta2 <- sta2 %>%
  rename(DateTime.EST = Date.Time..GMT.05.00,
         pres.water.kpa  = Abs.Pres..kPa..LGR.S.N..1303192..SEN.S.N..1303192.,
         temp.water.C = Temp...C..LGR.S.N..1303192..SEN.S.N..1303192.)
sta2$DateTime.EST <- as.POSIXct(sta2$DateTime.EST, tz = "EST",
                                format = "%m/%d/%y %I:%M:%S %p")
sta2 <- na.omit(sta2)
sta2$stageDevice <- "silver HOBO"
sta2$stageSerialNb <- "1303192"

# Combine HOBO data into single data frame
sta <- rbind(sta, sta2)

silversta <- sta

# 3. Read black body HOBO -------------------------------------------------

# Read tables from 2024 for black HOBO
staloc <- paste0(dataloc, "CLGB.AG/data/stage/y2024/rawData/")
flist <- list.files(staloc, pattern = "*_black.csv")
flist

dfloc <- paste0(staloc, flist[1])
sta <- read.csv(dfloc, skip = 1)
sta <- sta[2:4]
sta <- sta %>%
  rename(DateTime.EST = Date.Time..GMT.05.00,
         pres.water.kpa = Abs.Pres..kPa..LGR.S.N..22063078..SEN.S.N..22063078.,
         temp.water.C = Temp...C..LGR.S.N..22063078..SEN.S.N..22063078.)
sta$DateTime.EST <- as.POSIXct(sta$DateTime.EST, tz = "EST",
                           format = "%m/%d/%y %I:%M:%S %p")
sta <- na.omit(sta)
sta$stageDevice <- "black HOBO"
sta$stageSerialNb <- "22063078"

# read second black HOBO file
dfloc <- paste0(staloc, flist[2])
sta2 <- read.csv(dfloc, skip = 1)
sta2 <- sta2[2:4]
sta2 <- sta2 %>%
  rename(DateTime.EST = Date.Time..GMT.05.00,
         pres.water.kpa = Abs.Pres..kPa..LGR.S.N..22063078..SEN.S.N..22063078.,
         temp.water.C = Temp...C..LGR.S.N..22063078..SEN.S.N..22063078.)
sta2$DateTime.EST <- as.POSIXct(sta2$DateTime.EST, tz = "EST",
                               format = "%m/%d/%y %I:%M:%S %p")
sta2 <- na.omit(sta2)
sta2$stageDevice <- "black HOBO"
sta2$stageSerialNb <- "22063078"

# Combine HOBO data into single data frame
sta <- rbind(sta, sta2)

# read third black HOBO file
dfloc <- paste0(staloc, flist[3])
sta2 <- read.csv(dfloc, skip = 1)
sta2 <- sta2[2:4]
sta2 <- sta2 %>%
  rename(DateTime.EST = Date.Time..GMT.05.00,
         pres.water.kpa = Abs.Pres..kPa..LGR.S.N..22063078..SEN.S.N..22063078.,
         temp.water.C = Temp...C..LGR.S.N..22063078..SEN.S.N..22063078.)
sta2$DateTime.EST <- as.POSIXct(sta2$DateTime.EST, tz = "EST",
                                format = "%m/%d/%y %I:%M:%S %p")
sta2 <- na.omit(sta2)
sta2$stageDevice <- "black HOBO"
sta2$stageSerialNb <- "22063078"

# Combine HOBO data into single data frame
sta <- rbind(sta, sta2)

# read fourth black HOBO file
dfloc <- paste0(staloc, flist[4])
sta2 <- read.csv(dfloc, skip = 1)
sta2 <- sta2[2:4]
sta2 <- sta2 %>%
  rename(DateTime.EST = Date.Time..GMT.05.00,
         pres.water.kpa = Abs.Pres..kPa..LGR.S.N..20434532..SEN.S.N..20434532.,
         temp.water.C = Temp...C..LGR.S.N..20434532..SEN.S.N..20434532.)
sta2$DateTime.EST <- as.POSIXct(sta2$DateTime.EST, tz = "EST",
                                format = "%m/%d/%y %I:%M:%S %p")
sta2 <- na.omit(sta2)
sta2$stageDevice <- "black HOBO"
sta2$stageSerialNb <- "20434532"

# Combine HOBO data into single data frame
sta <- rbind(sta, sta2)

# Remove duplicated rows
sta <- sta[!duplicated(sta),]
blacksta <- sta

# 4. Read EXO  ------------------------------------------------------------

exoname <- paste(dataloc, "CLGB.AG/data/fdom/y2024/intermediateData/EXO_CLGBag_2024-05-02-to-2024-09-25.csv", sep = "")
exodat <- read.csv(exoname)

# Convert date and time columns to datetime
exodat$DateTime.EST <- as.POSIXct(paste(exodat$Date, exodat$Time), format = "%m/%d/%Y %H:%M:%S")

head(exodat)
exodat <- exodat[c("DateTime.EST", "Pressure.psi", "Temp.C")]
exodat$stageDevice <- "YSI EXO"
exodat$stageSerialNb <- "24C102050"

# Read data from second YSI EXO (it has a different pressure calibartion)
exoname <- paste(dataloc, "CLGB.AG/data/fdom/y2024/intermediateData/EXO_CLGBag_2024-10-04-to-2024-12-04.csv", sep = "")
exodat2 <- read.csv(exoname)
exodat2$DateTime.EST <- as.POSIXct(paste(exodat2$Date, exodat2$Time), format = "%m/%d/%Y %H:%M:%S")
head(exodat2)
exodat2 <- exodat2[c("DateTime.EST", "Pressure.psi", "Temp.C")]
exodat2$stageDevice <- "YSI EXO"
exodat2$stageSerialNb <- "12M100797"

# Combine all EXO data into single data frame
exodat <- rbind(exodat, exodat2)

# Remove duplicated rows
exodat <- exodat[!duplicated(exodat),]

# Round datetime to nearest 15 minutes
exodat$DateTime.EST <- round_date(exodat$DateTime.EST, "15 mins")

# Convert psi to kpa
exodat$pres.water.kpa <- exodat$Pressure.psi * 6.89476

exodat <- exodat %>%
  rename(temp.water.C = Temp.C)

exodat <- exodat[c("DateTime.EST", "temp.water.C", "pres.water.kpa", "stageDevice", "stageSerialNb")]

# Select water pressure to use --------------------------------------------
# Silver > black > exo

dat <- barodat
dat <- merge(barodat, silversta, by = "DateTime.EST", all = TRUE)
bldat <- merge(barodat, blacksta, by = "DateTime.EST", all = TRUE)
exdat <- merge(barodat, exodat, by = "DateTime.EST", all = TRUE)

# Fill in gaps with black HOBO data
dat <- dat %>% inner_join(bldat, by= c("DateTime.EST", "pres.baro.kpa", "temp.baro.C")) %>%
  mutate(pres.water.kpa = coalesce(pres.water.kpa.x, pres.water.kpa.y),
         temp.water.C = coalesce(temp.water.C.x, temp.water.C.y),
         stageDevice = coalesce(stageDevice.x, stageDevice.y),
         stageSerialNb = coalesce(stageSerialNb.x, stageSerialNb.y)) %>%
  select(DateTime.EST, pres.baro.kpa, temp.baro.C, pres.water.kpa, temp.water.C,
         stageDevice, stageSerialNb)

# Fill gaps with EXO data
dat <- dat %>% inner_join(exdat, by= c("DateTime.EST", "pres.baro.kpa", "temp.baro.C")) %>%
  mutate(pres.water.kpa = coalesce(pres.water.kpa.x, pres.water.kpa.y),
         temp.water.C = coalesce(temp.water.C.x, temp.water.C.y),
         stageDevice = coalesce(stageDevice.x, stageDevice.y),
         stageSerialNb = coalesce(stageSerialNb.x, stageSerialNb.y)) %>%
  select(DateTime.EST, pres.baro.kpa, temp.baro.C, pres.water.kpa, temp.water.C,
         stageDevice, stageSerialNb)

# 5. Convert pressure to depth --------------------------------------------

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

dat[sapply(dat, is.infinite)] <- NA


# Export data ----------------------------------------------------------
dat2 <- dat

dat$DateTime.EST <- as.character(format(dat$DateTime.EST))
write.csv(dat, paste0(dataloc,"/CLGB.AG/data/ratingCurve/CLGB.AG_2024_waterDepths.csv"), row.names = F)
write.csv(dat, paste0(dataloc,"/CLGB.AG/data/stage/y2024/finaldata/CLGB.AG_2024_waterDepths.csv"), row.names = F)
dat <- dat2

# Corrected water depths --------------------------------------------------

offset2024A <- 0.332121113 # 2024-05-02 to 2024-05-24 13:00
offsetchangedateA <- as.POSIXct("2024-05-24 13:00")

offset2024B <- 0.293621184 # 2024-05-24 13:15 to  2024-05-31 14:15
offsetchangedateB <- as.POSIXct("2024-05-31 14:15")

offset2024C <- 0.332121113 # 2024-05-31 14:30 to 2023-06-11 15:00
offsetchangedateC <- as.POSIXct("2024-06-11 15:00")

offset2024D <- 0.372121113 #  2024-06-11 16:00 to 2024-06-13 11:00
offsetchangedateD <- as.POSIXct("2024-06-13 11:00")

offset2024E <- 0.336845103 #  2024-06-13 11:00 to 2024-07-22 12:00
offsetchangedateE <- as.POSIXct("2024-07-22 12:00")

offset2024F <- 0.285065973 #  2024-07-22 12:00 to 2024-08-26 15:45
offsetchangedateF <- as.POSIXct("2024-08-26 10:45")

offset2024G <- 0.217462984 # Black HOBO starts 2024-08-26
offsetchangedateG <- as.POSIXct("2024-09-27 00:00")

offset2025 <- 0.067912854 # 2024-09-27 to 2025-06-02 silver HOBO
offsetchangedate2025 <- as.POSIXct("2024-09-27 00:00")


for (i in 1:nrow(dat)){
  if (dat$DateTime.EST[i] < offsetchangedateA){
    dat$stage.correction.factor.m[i] = offset2024A
  } else if (dat$DateTime.EST[i] < offsetchangedateB & dat$DateTime.EST[i] > offsetchangedateA){
    dat$stage.correction.factor.m[i] = offset2024B
  } else if (dat$DateTime.EST[i] < offsetchangedateC & dat$DateTime.EST[i] > offsetchangedateB){
    dat$stage.correction.factor.m[i] = offset2024C
  } else if (dat$DateTime.EST[i] < offsetchangedateD & dat$DateTime.EST[i] > offsetchangedateC){
    dat$stage.correction.factor.m[i] = offset2024D
  } else if (dat$DateTime.EST[i] < offsetchangedateE & dat$DateTime.EST[i] > offsetchangedateD){
    dat$stage.correction.factor.m[i] = offset2024E
  } else if (dat$DateTime.EST[i] < offsetchangedateF & dat$DateTime.EST[i] > offsetchangedateE){
    dat$stage.correction.factor.m[i] = offset2024F
  } else if (dat$DateTime.EST[i] < offsetchangedateG & dat$DateTime.EST[i] > offsetchangedateF){
    dat$stage.correction.factor.m[i] = offset2024G
  } else {
    dat$stage.correction.factor.m[i] = offset2025
  }
}

dat$stage.corrected.m <- dat$stage.raw.m + dat$stage.correction.factor.m
dat$stage.corrected.m[dat$stage.corrected.m < 0] <- NA


# Remove values from EXO going out of water
for (i in 1:nrow(dat)){
  k = i - 1
  j = i - 2
  m = i - 3
  n = i - 4
  p = i - 5
  if (i == 1){
    dat$diff <- NA
  } else if (!is.na(dat$stage.corrected.m[i]) & !is.na(dat$stage.corrected.m[k])){
    dat$diff[i] <- dat$stage.corrected.m[k] - dat$stage.corrected.m[i] 
  } else {
    dat$diff[i] <- NA
  }
  if (i <=2 ){
    dat$diff2 <- NA
  } else if (!is.na(dat$stage.corrected.m[i]) & !is.na(dat$stage.corrected.m[j])){
    dat$diff2[i] <- dat$stage.corrected.m[j] - dat$stage.corrected.m[i] 
  } else {
    dat$diff2[i] <- NA
  }
  if (i <= 3){
    dat$diff3 <- NA
  } else if (!is.na(dat$stage.corrected.m[i]) & !is.na(dat$stage.corrected.m[m])){
    dat$diff3[i] <- dat$stage.corrected.m[m] - dat$stage.corrected.m[i] 
  } else {
    dat$diff3[i] <- NA
  }
  if (i <= 4){
    dat$diff4 <- NA
  } else if (!is.na(dat$stage.corrected.m[i]) & !is.na(dat$stage.corrected.m[n])){
    dat$diff4[i] <- dat$stage.corrected.m[n] - dat$stage.corrected.m[i] 
  } else {
    dat$diff4[i] <- NA
  }
  if (i <= 5){
    dat$diff5 <- NA
  } else if (!is.na(dat$stage.corrected.m[i]) & !is.na(dat$stage.corrected.m[p])){
    dat$diff5[i] <- dat$stage.corrected.m[p] - dat$stage.corrected.m[i] 
  } else {
    dat$diff5[i] <- NA
  }
  
}

for (i in 1:nrow(dat)){
  if (!is.na(dat$diff[i]) & dat$diff[i] > 0.1){
    dat$stage.corrected.m[i] <- NA 
  }
  if (!is.na(dat$diff2[i]) & dat$diff2[i] > 0.1){
    dat$stage.corrected.m[i] <- NA 
  }
  if (!is.na(dat$diff3[i]) & dat$diff3[i] > 0.1){
    dat$stage.corrected.m[i] <- NA 
  }
  if (!is.na(dat$diff4[i]) & dat$diff4[i] > 0.1){
    dat$stage.corrected.m[i] <- NA 
  }
  if (!is.na(dat$diff5[i]) & dat$diff5[i] > 0.1){
    dat$stage.corrected.m[i] <- NA 
  }
}

plot(dat$DateTime.EST, dat$stage.corrected.m, type = "l")
dat <- subset(dat, select = -c(diff, diff2, diff3, diff4, diff5))

# Calculate discharge
for (i in 1:nrow(dat)){
  if(!is.na(dat$stage.corrected.m[i])){
    if(dat$stage.corrected.m[i] < 0.51){
      dat$Q.m3s[i] <- 0
      dat$Q.m3sQF[i] <- 0
    } else if (dat$stage.corrected.m[i] < 0.74){
      dat$Q.m3s[i] <- 10.037 * dat$stage.corrected.m [i]**16.081
      dat$Q.m3sQF[i] <- 0
    } else {
      dat$Q.m3s[i] <- 0.2281 * dat$stage.corrected.m [i]**4.2561
      if(dat$stage.corrected.m [i] < 0.91){
        dat$Q.m3sQF[i] <- 0
      } else {
        dat$Q.m3sQF[i] <- 1
      }
    }
  } else{
    dat$Q.m3s[i] <- NA
    dat$Q.m3sQF[i] <- 1
  }
}

dat[sapply(dat, is.infinite)] <- NA

plot(dat$DateTime.EST, dat$Q.m3s, type = "l")

# Save data
dat2 <- dat
dat$DateTime.EST <- as.character(format(dat$DateTime.EST))
write.csv(dat, paste0(dataloc,"/CLGB.AG/data/ratingCurve/CLGB.AG_2024_DISCHARGE.csv"), row.names = F)
write.csv(dat, paste0(dataloc,"/CLGB.AG/data/stage/y2024/finalData/CLGB.AG_2024_DISCHARGE.csv"), row.names = F)
dat <- dat2

# Import Q measurements ---------------------------------------------------

dat <- read.csv(paste0(dataloc,"/CLGB.AG/data/stage/y2024/finalData/CLGB.AG_2024_DISCHARGE.csv"))
dat$DateTime.EST <- as.POSIXct(dat$DateTime.EST, tz = "EST",
                               format = "%Y-%m-%d %H:%M:%S")

qmeas <- read.csv(paste0(dataloc, "CLGB.AG/data/ratingCurve/discharge_measurements.csv"))
qmeas$DateTime.EST <- as.POSIXct(paste(qmeas$date, qmeas$time), tz = "EST",
                                 format = "%Y-%m-%d %H:%M")
qmeas <- subset(qmeas, select =c("DateTime.EST", "measured.Q.m3s"))
dat <- merge(dat, qmeas, by = "DateTime.EST", all.x = TRUE, all.y = FALSE)

dat$Q.m3s[dat$Q.m3s < 1e-10] <- NA
ggplot(dat, aes(x=DateTime.EST)) +
  geom_line(aes(y = Q.m3s))+
  geom_point(aes(y = measured.Q.m3s,  color = "measured Q"))+
  scale_x_datetime(name = "Date 2024") +
  scale_y_log10( name = "Discharge (m3/s)", limits = c(0.0001, 1))+
  theme_classic()



