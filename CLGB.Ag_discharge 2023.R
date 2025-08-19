# CLGB.Ag 2023 discharge

library(tidyverse)
library(ggplot2)

# Set data location
dataloc <- "C:/Users/laram/OneDrive - USNH/CLGB/"


# 1. Read barometric data -------------------------------------------------

# Read barometrice pressure data
# Get this data from Jody Potter at WQAL, make sure that units are the same 
baroloc <- paste(dataloc, "WHB Barometric/annual/WHB_atm_Pressure2023.csv", sep = "")
barodat <- read.csv(baroloc)
barodat$datetime <- as.POSIXct(barodat$datetime, tz = "EST",
                           format = "%Y-%m-%d %H:%M")
barodat <- barodat %>%
  rename(DateTime.EST = datetime,
         pres.baro.kpa = pres.kpa,
         temp.baro.C = temp.C)
barodat <- barodat[2:4]
barodat <- na.omit(barodat)

# 2. Read silver body HOBO ------------------------------------------------

# Read tables from 2023 for silver body HOBO
staloc <- paste0(dataloc, "CLGB.AG/data/stage/y2023/rawData/")
flist <- list.files(staloc, pattern = "*.csv")
flist

# Read the first stage file
dfloc <- paste0(staloc, flist[1])
sta <- read.csv(dfloc, skip = 1)
sta <- sta[2:4]
sta <- sta %>%
  rename(DateTime.EST = Date.Time..GMT.05.00,
         pres.water.kpa = Abs.Pres..kPa..LGR.S.N..1303191..SEN.S.N..1303191.,
         temp.water.C = Temp...C..LGR.S.N..1303191..SEN.S.N..1303191.)
sta$DateTime.EST <- as.POSIXct(sta$DateTime.EST, tz = "EST",
                           format = "%m/%d/%y %I:%M:%S %p")
sta <- na.omit(sta)
sta$stageDevice <- "silver HOBO"
sta$stageSerialNb <- "1303191"

# Read the second stage file
dfloc <- paste0(staloc, flist[2])
sta2 <- read.csv(dfloc, skip = 1)
sta2 <- sta2[2:4]
sta2 <- sta2 %>%
  rename(DateTime.EST = Date.Time..GMT.05.00,
         pres.water.kpa = Abs.Pres..kPa..LGR.S.N..1303191..SEN.S.N..1303191.,
         temp.water.C = Temp...C..LGR.S.N..1303191..SEN.S.N..1303191.)
sta2$DateTime.EST <- as.POSIXct(sta2$DateTime.EST, tz = "EST",
                               format = "%m/%d/%y %I:%M:%S %p")
sta2 <- na.omit(sta2)
sta2$stageDevice <- "silver HOBO"
sta2$stageSerialNb <- "1303191"

# Combine HOBO data into single data frame
sta <- rbind(sta, sta2)

# Read the third stage file
dfloc <- paste0(staloc, flist[3])
sta2 <- read.csv(dfloc, skip = 1)
sta2 <- sta2[2:4]
sta2 <- sta2 %>%
  rename(DateTime.EST = Date.Time..GMT.05.00,
         pres.water.kpa = Abs.Pres..kPa..LGR.S.N..1303191..SEN.S.N..1303191.,
         temp.water.C = Temp...C..LGR.S.N..1303191..SEN.S.N..1303191.)
sta2$DateTime.EST <- as.POSIXct(sta2$DateTime.EST, tz = "EST",
                                format = "%m/%d/%y %I:%M:%S %p")
sta2 <- na.omit(sta2)
sta2$stageDevice <- "silver HOBO"
sta2$stageSerialNb <- "1303191"

# Combine HOBO data into single data frame
sta <- rbind(sta, sta2)

# Read the fourth stage file
dfloc <- paste0(staloc, flist[4])
sta2 <- read.csv(dfloc, skip = 1)
sta2 <- sta2[2:4]
sta2 <- sta2 %>%
  rename(DateTime.EST = Date.Time..GMT.05.00,
         pres.water.kpa = Abs.Pres..kPa..LGR.S.N..1303191..SEN.S.N..1303191.,
         temp.water.C = Temp...C..LGR.S.N..1303191..SEN.S.N..1303191.)
sta2$DateTime.EST <- as.POSIXct(sta2$DateTime.EST, tz = "EST",
                                format = "%m/%d/%y %I:%M:%S %p")
sta2 <- na.omit(sta2)
sta2$stageDevice <- "silver HOBO"
sta2$stageSerialNb <- "1303191"

# Combine HOBO data into single data frame
sta <- rbind(sta, sta2)

# Merge baro and silver HOBO data
dat <- merge(barodat, sta, by = "DateTime.EST", all = FALSE)

# Convert pressure to depth --------------------------------------------

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

dat[sapply(dat, is.infinite)] <- NA

# Export data ----------------------------------------------------------
dat2 <- dat
dat$DateTime.EST <- as.character(format(dat$DateTime.EST))
write.csv(dat, paste0(dataloc,"/CLGB.AG/data/ratingCurve/CLGB.AG_2023_waterDepths.csv"), row.names = F)
write.csv(dat, paste0(dataloc,"/CLGB.AG/data/stage/y2023/finaldata/CLGB.AG_2023_waterDepths.csv"), row.names = F)
dat <- dat2

# Convert depths to standardized depths
offset2023A <- -0.035325772 # 2022 to 2023-06-01
offset2023B <- -0.077775347 # 2023-06-01 10:45 to 2023-10-06

offsetchangedate <- as.POSIXct("2023-06-01 10:45")
baddepthdate <- as.POSIXct("2023-11-13 00:00")

# Add depth offset based on date
for (i in 1:nrow(dat)){
  if (dat$DateTime.EST[i] < offsetchangedate){
    dat$stage.correction.factor.m[i] = offset2023A
  } else if (dat$DateTime.EST[i] < baddepthdate) {
    dat$stage.correction.factor.m[i] = offset2023B
  } else {
    dat$stage.correction.factor.m[i] = NA
    
  }
}


dat$stage.corrected.m <- dat$stage.raw.m + dat$stage.correction.factor.m

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

dat$Q.m3s[dat$Q.m3s < 1e-22] <- NA

plot(dat$DateTime.EST, dat$Q.m3s, type = "l",
     ylab = "Q (m3/s)", xlab = "Date 2023")
plot(dat$DateTime.EST, dat$Q.m3s, type = "l", log = "y",
     ylab = "Q (m3/s)", xlab = "Date 2023")

dat2 <- dat
dat$DateTime.EST <- as.character(format(dat$DateTime.EST))
write.csv(dat, paste0(dataloc,"/CLGB.AG/data/ratingCurve/CLGB.AG_2023_DISCHARGE.csv"), row.names = F)
write.csv(dat, paste0(dataloc,"/CLGB.AG/data/stage/y2023/finalData/CLGB.AG_2023_DISCHARGE.csv"), row.names = F)
dat <- dat2

# Import Q measurements ---------------------------------------------------

dat <- read.csv(paste0(dataloc,"/CLGB.AG/data/stage/y2023/finalData/CLGB.AG_2023_DISCHARGE.csv"))
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
  scale_x_datetime(name = "Date 2023") +
  scale_y_log10( name = "Discharge (m3/s)", limits = c(0.0001, 1))+
  theme_classic()

